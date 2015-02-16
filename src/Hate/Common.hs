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
    , getKey
    , whenKeyPressed
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

import Control.Concurrent (threadDelay)

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

hateInitWindow :: String -> (Int, Int) -> IO G.Window
hateInitWindow titl (width, height) = do
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

            hateInitGL

            return win

hateInitGL = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

hateSuccessfulExit :: G.Window -> IO b
hateSuccessfulExit win = do
    G.destroyWindow win
    G.terminate
    exitSuccess          

hateLoop :: HateInner us ()
hateLoop = do
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
        runHateDraw $ renderBatch drawRequests

        -- update the game state in constant intervals
        Just t <- liftIO G.getTime
        let tDiff = t - (lastUpdateTime gs)

        let desiredFPS = 60.0
        let desiredSPF = 1.0 / desiredFPS

        when (tDiff > desiredSPF) $ do
            runHate $ updateFn gs
            modify $ \x -> x { lastUpdateTime = t }

        when (tDiff < desiredSPF) $ liftIO $        
            threadDelay (floor $ 1000000 * (desiredSPF - tDiff))

        liftIO $ do 
            G.swapBuffers w
            G.pollEvents
        hateLoop 

getKey :: G.Key -> Hate us Bool
getKey k = UnsafeHate $ do
    gs <- get
    stt <- liftIO $ G.getKey (window gs) k 
    return $ keystateToBool stt
    where keystateToBool s
            | s == G.KeyState'Released = False
            | otherwise = True

whenKeyPressed :: G.Key -> Hate us () -> Hate us ()
whenKeyPressed k action = do
    b <- getKey k
    if b then action 
         else return ()

initialLibraryState = LibraryState <$> initialGraphicsState

runApp :: Config -> LoadFn us -> UpdateFn us -> DrawFn us -> IO ()
runApp config ldFn upFn drFn = do
    win <- hateInitWindow (windowTitle config) (windowSize config)
    libS <- initialLibraryState

    print $! "Loading user state"
    initialUserState <- ldFn

    time <- fromJust <$> G.getTime
    evalStateT hateLoop $ HateState { userState = initialUserState, window = win, drawFn = drFn, updateFn = upFn, libraryState = libS, lastUpdateTime = time }
    hateSuccessfulExit win    
