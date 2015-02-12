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

import Hate.Graphics.Util (initialGraphicsState)
import Hate.Graphics.Rendering

import Control.Monad.State
import Control.Applicative

import System.Exit
import System.IO

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

import Data.Maybe

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
        G.windowHint (G.WindowHint'ContextVersionMajor 4)
        G.windowHint (G.WindowHint'ContextVersionMinor 5)
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

        -- call user drawing function
        let drawRequests = drawFn gs $ userState gs
        runHateDraw $ mapM_ render drawRequests

        -- update the game state in constant intervals
        Just t <- liftIO G.getTime
        let tDiff = t - (lastUpdateTime gs)

        -- magic constant anyone
        when (tDiff > (1.0/60.0)) $ do
            runHate $ updateFn gs
            modify $ \x -> x { lastUpdateTime = t }

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

initialLibraryState = LibraryState <$> initialGraphicsState

runApp :: Config -> LoadFn us -> UpdateFn us -> DrawFn us -> IO ()
runApp config ldFn upFn drFn = do
    win <- glishaInitWindow (windowTitle config) (windowSize config)
    libS <- initialLibraryState

    print $! "Loading user state"
    initialUserState <- ldFn

    time <- fromJust <$> G.getTime
    evalStateT glishaLoop $ HateState { userState = initialUserState, window = win, drawFn = drFn, updateFn = upFn, libraryState = libS, lastUpdateTime = time }
    glishaSuccessfulExit win
