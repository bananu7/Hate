{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
import Hate.Events

import Hate.Graphics.Rendering
import Hate.Graphics.Backend

import Control.Monad.State
import Control.Applicative

import System.Exit
import System.IO

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G

import Data.Maybe
import Control.Lens
import Control.Concurrent (threadDelay)

stderrErrorCallback :: G.ErrorCallback
stderrErrorCallback _ = hPutStrLn stderr
 
--keyCallback :: G.KeyCallback
--keyCallback win key _ action _ =
--    when (key == G.Key'Escape && action == G.KeyState'Pressed) $
--        G.setWindowShouldClose win True        

choose :: [IO (Maybe a)] -> IO (Maybe a)
choose [] = return Nothing
choose (x:xs) = do
    a <- x
    if isJust a
        then return a -- short-circuit
        else choose xs -- eventually fall to Nothing

data GlContextDescriptor = GlContextDescriptor {
    majVersion :: Int,
    minVersion :: Int,
    forwardCompat :: Bool
} deriving Show


hateInitWindow :: String -> (Int, Int) -> IO G.Window
hateInitWindow titl wSize = do
    G.setErrorCallback (Just stderrErrorCallback)
    successfulInit <- G.init
    -- if init failed, we exit the program
    bool successfulInit (hateFailureExit "GLFW Init failed") $ do
        mw <- choose
            [ tryOpenWindow (GlContextDescriptor 4 5 True) wSize titl
            , tryOpenWindow (GlContextDescriptor 4 5 False) wSize titl
            , tryOpenWindow (GlContextDescriptor 4 4 True) wSize titl
            , tryOpenWindow (GlContextDescriptor 3 3 True) wSize titl
            , tryOpenWindow (GlContextDescriptor 3 3 False) wSize titl
            ]

        case mw of
            Nothing -> G.terminate >> (hateFailureExit "Window creation failed")
            Just win -> do
                G.makeContextCurrent mw
                G.swapInterval 1 --vsync
                hateInitGL
                return win

tryOpenWindow :: GlContextDescriptor -> (Int, Int) -> String -> IO (Maybe G.Window)
tryOpenWindow cd (width, height) titl = do
    putStrLn $ "Opening Window (" ++ show cd ++ ")"

    G.windowHint (G.WindowHint'ContextVersionMajor (majVersion cd))
    G.windowHint (G.WindowHint'ContextVersionMinor (minVersion cd))
    G.windowHint (G.WindowHint'OpenGLForwardCompat (forwardCompat cd))
    G.windowHint (G.WindowHint'OpenGLProfile G.OpenGLProfile'Core)
    G.windowHint (G.WindowHint'OpenGLDebugContext True)
    G.createWindow width height titl Nothing Nothing

hateInitGL :: IO ()
hateInitGL = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

hateFailureExit :: String -> IO a
hateFailureExit errMsg = do
    hPutStrLn stderr errMsg
    exitFailure

hateSuccessfulExit :: G.Window -> IO b
hateSuccessfulExit win = do
    G.destroyWindow win
    G.terminate
    exitSuccess

updateRendererState :: (forall r. Renderer r => (r -> IO (a, r))) -> HateInner us a
updateRendererState mutator = zoom (libraryState.graphicsState) (StateT mutator)

runHateDraw :: (forall r. Renderer r => StateT r IO a) -> HateInner us a
runHateDraw m = updateRendererState (runStateT m)

hateLoop :: HateInner us ()
hateLoop = do
    gs <- get
    let w = gs ^. window

    shouldClose <- (liftIO . G.windowShouldClose) w
    unless shouldClose $ do 
        liftIO $ do
            (width, height) <- G.getFramebufferSize w
            --let ratio = fromIntegral width / fromIntegral height
    
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
            GL.clear [GL.ColorBuffer]

        -- call user drawing function
        let drawRequests = gs ^. drawFn $ gs ^. userState
        runHateDraw $ render drawRequests

        -- update the game state in constant intervals
        Just t <- liftIO G.getTime
        let tDiff = t - (gs ^. lastUpdateTime)

        let desiredFPS = 60.0
        let desiredSPF = 1.0 / desiredFPS

        when (tDiff > desiredSPF) $ do
            evts <- reverse <$> fetchEvents
            let allowedEvts = filterEventsForEndUser evts

            handleInternalEvents evts

            -- print all the events out;
            -- leaving as dead code because might someday be helpful in debug
            --liftIO $ mapM print evts

            runHate $ (gs ^. updateFn) allowedEvts
            lastUpdateTime .= t

        when (tDiff < desiredSPF) $ liftIO $
            threadDelay (floor $ 1000000 * (desiredSPF - tDiff))

        liftIO $ do 
            G.swapBuffers w
            G.pollEvents
        hateLoop 

handleInternalEvents :: [Event] -> HateInner us ()
handleInternalEvents = mapM_ handleEvent
    where
        handleEvent e = case e of
            EventWindowSize xs ys -> do
                liftIO $ GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral xs) (fromIntegral ys))
                runHateDraw $ updateScreenSize (xs, ys)
            _ -> return ()

getKey :: G.Key -> Hate us Bool
getKey k = UnsafeHate $ do
    gs <- get
    stt <- liftIO $ G.getKey (gs ^. window) k 
    return $ keystateToBool stt
    where keystateToBool s
            | s == G.KeyState'Released = False
            | otherwise = True

whenKeyPressed :: G.Key -> Hate us () -> Hate us ()
whenKeyPressed k action = do
    b <- getKey k
    if b then action 
         else return ()

-- This function is called after the window has been created
pickRenderer :: (Int, Int) -> IO RendererI
pickRenderer ws = do
    (G.Version vMaj vMin _) <- G.getVersion
    if vMaj >= 4
        then initialRendererStateModern ws
        else initialRendererStateCompat ws

initialLibraryState :: Config -> IO LibraryState
initialLibraryState cfg = LibraryState <$> pickRenderer (windowSize cfg)
                                       <*> initialEventsState

runApp :: Config -> LoadFn us -> UpdateFn us -> DrawFn us -> IO ()
runApp config ldFn upFn drFn = do
    win <- hateInitWindow (windowTitle config) (windowSize config)
    libState <- initialLibraryState config

    setCallbacks (libState ^. eventsState) win

    print $! "Loading user state"
    initialUserState <- ldFn

    time <- fromJust <$> G.getTime
    evalStateT hateLoop $ HateState initialUserState libState win drFn upFn time
    hateSuccessfulExit win    
