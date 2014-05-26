{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : GlishaCommon
Description : Core and common parts of the Glisha framework.
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental
Portability : requires OpenGL and GLFW build

-}

module Glisha.Common where 

import Control.Monad.State

import System.Exit
import System.IO

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

import Glisha.Util 

-- GlishaInner is the inner Glisha state used by the API
data GlishaState us = GlishaState { userState :: us, window :: G.Window, drawFn :: DrawFn us }
type GlishaInner us a = StateT (GlishaState us) IO a

-- |Glisha Monad restricts user operations
newtype Glisha us a = UnsafeGlisha { runGlisha :: GlishaInner us a }
instance Monad (Glisha us) where
    return = UnsafeGlisha . return
    (UnsafeGlisha m) >>= k = UnsafeGlisha $ m >>= runGlisha . k

{- |This is one of the two functions that the user has to
 - provide in order to use the framework. It's a regular IO
 - function, so it's not limited in any way. It has to produce
 - initial state of the user's program. -}
type LoadFn userStateType = IO userStateType
{- |The main framework update function runs in the restricted
 - Glisha context. Only safe Glisha functions can be used inside.
 - Because Glisha is an instance of MonadState, it can be treated
 - just as the State monad with the registered user data. -}
type DrawFn us = Glisha us () 

-- Type classes for 2D and 3D implementations

-- |Anything that can be drawn, basically
class Drawable d where
    draw :: d -> IO ()

{-
type KeyCallbackFn us = G.Key -> StateT us IO ()
data Callbacks us = Callbacks { onKeyUp :: KeyCallbackFn us, onKeyDown :: KeyCallbackFn }
emptyKeyCallback _ = return ()
defaultCallbacks = Callbacks { onKeyUp = emptyKeyCallback, onKeyDown = emptyKeyCallback }
-}

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description
 
keyCallback :: G.KeyCallback
keyCallback window key scancode action mods =
    when (key == G.Key'Escape && action == G.KeyState'Pressed) $
        G.setWindowShouldClose window True        

glishaInitWindow :: IO G.Window
glishaInitWindow = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      G.windowHint (G.WindowHint'ContextVersionMajor 3)
      G.windowHint (G.WindowHint'ContextVersionMinor 3)
      G.windowHint (G.WindowHint'OpenGLForwardCompat True)
      G.windowHint (G.WindowHint'OpenGLProfile G.OpenGLProfile'Core)
      G.windowHint (G.WindowHint'OpenGLDebugContext True)
 
      mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          G.setKeyCallback window (Just keyCallback)
          return window

glishaSuccessfulExit window = do
    G.destroyWindow window
    G.terminate
    exitSuccess          

glishaLoop :: GlishaInner us ()
glishaLoop = do
    gs <- get
    let w = window gs

    shouldClose <- (liftIO . G.windowShouldClose) w
    if not shouldClose then do 
        liftIO $ do
            (width, height) <- G.getFramebufferSize w
            let ratio = fromIntegral width / fromIntegral height
    
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

      else return ()

glishaGetKey :: G.Key -> Glisha us Bool
glishaGetKey k = UnsafeGlisha $ do
    gs <- get
    state <- liftIO $ G.getKey (window gs) k 
    return $ keystateToBool state
    where keystateToBool s
            | s == G.KeyState'Released = False
            | otherwise = True

instance MonadState us (Glisha us) where
    get = UnsafeGlisha $ do
            gs <- get
            return $ userState gs

    put s = UnsafeGlisha $ do
              gs <- get
              put $ gs { userState = s }
   
glishaDraw :: Drawable a => a -> Glisha us ()
glishaDraw d = UnsafeGlisha $ liftIO $ draw d
            
runApp :: LoadFn us -> DrawFn us -> IO ()
runApp loadFn drawFn = do
    window <- glishaInitWindow
    initialUserState <- loadFn

    evalStateT glishaLoop $ GlishaState initialUserState window drawFn

    glishaSuccessfulExit window

