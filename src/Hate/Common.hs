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

import Control.Concurrent (threadDelay)

-- Assuming this doesn't change, for now
-- this actually controls update rate, as the display
-- rate is vsynced
desiredFPS = 60.0
desiredSPF = 1.0 / desiredFPS

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

-- this function is so generic because it used to work with universally
-- quantified Renderer. Now that libraryState uses RendererI it's not strictly
-- necessary, but I don't mind leaving it here either.
updateRendererState :: (forall r. Renderer r => (r -> IO (a, r))) -> HateInner us a
updateRendererState mutator = do
    g <- gets libraryState
    case g of
        (LibraryState{ graphicsState = gs, ..}) -> do
            (ret, ngs) <- liftIO $ mutator gs
            modify $ \g -> g { libraryState = LibraryState { graphicsState = ngs, .. }}
            return $ ret

runHateDraw :: (forall r. Renderer r => StateT r IO a) -> HateInner us a
runHateDraw m = updateRendererState (runStateT m)

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
        runHateDraw $ render drawRequests

        -- update the game state in constant intervals
        Just t <- liftIO G.getTime
        let tDiff = t - (lastUpdateTime gs)

        evts <- reverse <$> fetchEvents
        let groupedEvts = groupEventsByTime (lastUpdateTime gs) desiredSPF evts

        when (tDiff > desiredSPF) $ if length groupedEvts > 0
            then mapM_ hateUpdate groupedEvts
            else hateUpdate []


        when (tDiff < desiredSPF) $ liftIO $
            threadDelay (floor $ 1000000 * (desiredSPF - tDiff))

        liftIO $ do 
            G.swapBuffers w
            G.pollEvents
        hateLoop

groupEventsByTime :: Time -> Time -> [TimedEvent] -> [[TimedEvent]]
groupEventsByTime _ _ [] = []
groupEventsByTime lastTime frameLength evts = current : groupEventsByTime (lastTime + frameLength) frameLength next
    where
        (current, next) = span inCurrentFrame evts
        inCurrentFrame = ((lastTime + frameLength) >) . fst

hateUpdate :: [TimedEvent] -> HateInner us ()
hateUpdate evts = do
    handleInternalEvents . map snd $ evts

    -- print all the events out;
    -- leaving as dead code because might someday be helpful in debug
    liftIO $ mapM print evts

    let allowedEvts = filter allowedEvent . map snd $ evts

    gs <- get
    runHate $ (updateFn gs) allowedEvts
    modify $ \x -> x { lastUpdateTime = lastUpdateTime x + desiredSPF }

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

    setCallbacks (eventsState libState) win

    print $! "Loading user state"
    initialUserState <- ldFn

    time <- fromJust <$> G.getTime
    evalStateT hateLoop $ HateState { userState = initialUserState, window = win, drawFn = drFn, updateFn = upFn, libraryState = libState, lastUpdateTime = time }
    hateSuccessfulExit win    
