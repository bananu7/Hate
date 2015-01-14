{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : GlishaCommon
Description : Core and common parts of the Glisha framework.
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental
Portability : requires OpenGL and GLFW build

-}

module Glisha.Common 
    ( module Glisha.Common
    , module Control.Monad.State
    ) where 

import Control.Monad.State
import Control.Applicative

import System.Exit
import System.IO

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

import Glisha.Util 

{-| Configuration object to pass to `runApp` -}
data Config
    = Config
        { windowTitle :: String
        , windowSize :: (Int, Int)
        } deriving (Eq, Show)

-- GlishaInner is the inner Glisha state used by the API
data GlishaState us libs = GlishaState { 
  userState :: us,
  libraryState :: libs,
  window :: G.Window,
  drawFn :: DrawFn us
}
type GlishaInner us libs a = StateT (GlishaState us libs) IO a

-- |Glisha Monad restricts user operations
newtype Glisha us libs a = UnsafeGlisha { runGlisha :: GlishaInner us libs a }
  deriving (Functor, Applicative, Monad)

{-instance Monad (Glisha us libs) where
    return = UnsafeGlisha . return
    (UnsafeGlisha m) >>= k = UnsafeGlisha $ m >>= runGlisha . k-}

instance MonadState us (Glisha us libs) where
    get = UnsafeGlisha $ do
            gs <- get
            return $ userState gs

    put s = UnsafeGlisha $ do
            gs <- get
            put $ gs { userState = s }

instance MonadState libs (Glisha us libs) where
    get = UnsafeGlisha $ do
            gs <- get
            return $ libraryState gs

    put s = UnsafeGlisha $ do
            gs <- get
            put $ gs { libraryState = s }

{- |This is one of the two functions that the user has to
 - provide in order to use the framework. It's a regular IO
 - function, so it's not limited in any way. It has to produce
 - initial state of the user's program. -}
type LoadFn userStateType = IO userStateType
{- |The main framework update function runs in the restricted
 - Glisha context. Only safe Glisha functions can be used inside.
 - Because Glisha is an instance of MonadState, it can be treated
 - just as the State monad with the registered user data. -}
type DrawFn us = forall libs. Glisha us libs () 

-- Type classes for 2D and 3D implementations

-- |Anything that can be drawn, basically
class Drawable d where
    draw :: forall us. forall libs. d -> Glisha us libs ()

{-
type KeyCallbackFn us = G.Key -> StateT us IO ()
data Callbacks us = Callbacks { onKeyUp :: KeyCallbackFn us, onKeyDown :: KeyCallbackFn }
emptyKeyCallback _ = return ()
defaultCallbacks = Callbacks { onKeyUp = emptyKeyCallback, onKeyDown = emptyKeyCallback }
-}

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback _ = hPutStrLn stderr
 
keyCallback :: G.KeyCallback
keyCallback win key _ action _ =
    when (key == G.Key'Escape && action == G.KeyState'Pressed) $
        G.setWindowShouldClose win True        

glishaInitWindow :: String -> (Int, Int) -> IO G.Window
glishaInitWindow titl (width, height) = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      G.windowHint (G.WindowHint'ContextVersionMajor 3)
      G.windowHint (G.WindowHint'ContextVersionMinor 3)
      G.windowHint (G.WindowHint'OpenGLForwardCompat True)
      G.windowHint (G.WindowHint'OpenGLProfile G.OpenGLProfile'Core)
      G.windowHint (G.WindowHint'OpenGLDebugContext True)
 
      mw <- G.createWindow width height titl Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \win -> do
          G.makeContextCurrent mw
          G.setKeyCallback win (Just keyCallback)
          return win

glishaSuccessfulExit :: G.Window -> IO b
glishaSuccessfulExit win = do
    G.destroyWindow win
    G.terminate
    exitSuccess          

glishaLoop :: GlishaInner us libs ()
glishaLoop = do
    gs <- get
    let w = window gs

    shouldClose <- (liftIO . G.windowShouldClose) w
    unless shouldClose $ do 
        liftIO $ do
            (width, height) <- G.getFramebufferSize w
            --let ratio = fromIntegral width / fromIntegral height
    
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
            GL.clear [GL.ColorBuffer]
    
        -- todo: Time measurements; -- Just t <- G.getTime ...

        -- call user drawing function
        let dfn = drawFn gs        
        runGlisha dfn     

        liftIO $ do 
            G.swapBuffers w
            G.pollEvents
        glishaLoop 

getKey :: G.Key -> Glisha us libs Bool
getKey k = UnsafeGlisha $ do
    gs <- get
    stt <- liftIO $ G.getKey (window gs) k 
    return $ keystateToBool stt
    where keystateToBool s
            | s == G.KeyState'Released = False
            | otherwise = True
   
runAppInner :: a -> Config -> LoadFn us -> DrawFn us -> IO ()
runAppInner libCfg config ldFn drFn = do
    win <- glishaInitWindow (windowTitle config) (windowSize config)
    initialUserState <- ldFn
    evalStateT glishaLoop $ GlishaState { userState = initialUserState, window = win, drawFn = drFn, libraryState = libCfg }
    glishaSuccessfulExit win
