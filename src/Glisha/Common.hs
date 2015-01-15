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
    ( module Control.Monad.State
    , module Glisha.Common.Types
    , Glisha.Common.Types.Glisha(..)
    ) where

import Control.Monad.State

import System.Exit
import System.IO

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

import Glisha.Util 
import Glisha.Common.Types

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

glishaLoop :: GlishaInner us ()
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

getKey :: G.Key -> Glisha us Bool
getKey k = UnsafeGlisha $ do
    gs <- get
    stt <- liftIO $ G.getKey (window gs) k 
    return $ keystateToBool stt
    where keystateToBool s
            | s == G.KeyState'Released = False
            | otherwise = True
   
runAppInner :: IO LibraryState -> Config -> LoadFn us -> DrawFn us -> IO ()
runAppInner libCfgM config ldFn drFn = do
    libCfg <- libCfgM
    win <- glishaInitWindow (windowTitle config) (windowSize config)
    initialUserState <- ldFn
    evalStateT glishaLoop $ GlishaState { userState = initialUserState, window = win, drawFn = drFn, libraryState = libCfg }
    glishaSuccessfulExit win
