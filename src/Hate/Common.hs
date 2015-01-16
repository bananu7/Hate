{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : HateCommon
Description : Core and common parts of the Hate framework.
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental
Portability : requires OpenGL and GLFW build
-}

module Hate.Common 
    ( module Hate.Common.Types
    , module Hate.Common.Instances
    , Hate.Common.Types.Hate(..)    
    , runApp
    ) where

import Hate.Util 
import Hate.Common.Types
import Hate.Common.Instances()

import Control.Monad.State

import System.Exit
import System.IO

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

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

glishaLoop :: HateInner us ()
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
        runHate dfn     

        liftIO $ do 
            G.swapBuffers w
            G.pollEvents
        glishaLoop 

getKey :: G.Key -> Hate us Bool
getKey k = UnsafeHate $ do
    gs <- get
    stt <- liftIO $ G.getKey (window gs) k 
    return $ keystateToBool stt
    where keystateToBool s
            | s == G.KeyState'Released = False
            | otherwise = True
   

runApp :: Config -> LoadFn us -> DrawFn us -> IO ()
runApp config ldFn drFn = do
    let libCfg = 0
    win <- glishaInitWindow (windowTitle config) (windowSize config)
    initialUserState <- ldFn
    evalStateT glishaLoop $ HateState { userState = initialUserState, window = win, drawFn = drFn, libraryState = libCfg }
    glishaSuccessfulExit win
