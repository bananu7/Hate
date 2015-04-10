module Hate.Events
    ( initialEventsState
    , setCallbacks
    , fetchEvents
    , filterEventsForEndUser
    , module Hate.Events.Types
    )
where

import qualified Graphics.UI.GLFW as GLFW

import Control.Lens

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Hate.Events.Types
import Hate.Common.Types

import Control.Monad.IO.Class (liftIO)

import GHC.Float (double2Float)

initialEventsState :: IO EventsState
initialEventsState = newTQueueIO :: IO (TQueue Event)

{- The code has been borrowed from GLFW-b-demo; thanks @bsl -}

-- I assume only one window can be used by the framework

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc _ x y          = atomically $ writeTQueue tc $ EventWindowPos       x y
windowSizeCallback      tc _ w h          = atomically $ writeTQueue tc $ EventWindowSize      w h
windowCloseCallback     tc _              = atomically $ writeTQueue tc $ EventWindowClose
windowRefreshCallback   tc _              = atomically $ writeTQueue tc $ EventWindowRefresh
windowFocusCallback     tc _ fa           = atomically $ writeTQueue tc $ EventWindowFocus     fa
windowIconifyCallback   tc _ ia           = atomically $ writeTQueue tc $ EventWindowIconify   ia
framebufferSizeCallback tc _ w h          = atomically $ writeTQueue tc $ EventFramebufferSize w h
mouseButtonCallback     tc _ mb mba mk    = atomically $ writeTQueue tc $ EventMouseButton     mb mba mk
cursorPosCallback       tc _ x y          = atomically $ writeTQueue tc $ EventCursorPos       (double2Float x) (double2Float y)
cursorEnterCallback     tc _ ca           = atomically $ writeTQueue tc $ EventCursorEnter     ca
scrollCallback          tc _ x y          = atomically $ writeTQueue tc $ EventScroll          x y
keyCallback             tc _ k sc ka mk   = atomically $ writeTQueue tc $ EventKey             k sc ka mk
charCallback            tc _ c            = atomically $ writeTQueue tc $ EventChar            c

setErrorCallback :: TQueue Event -> IO ()
setErrorCallback eventsChan = GLFW.setErrorCallback $ Just $ errorCallback eventsChan

setCallbacks :: EventsState -> GLFW.Window -> IO ()
setCallbacks eventsChan win = do
    GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
    GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
    GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
    GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
    GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
    GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
    GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
    GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
    GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
    GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
    GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

fetchEvents :: HateInner us [Event]
fetchEvents = fetchEvents' []
    where 
        fetchEvents' :: [Event] -> HateInner us [Event]
        fetchEvents' xs = do
            tc <- use (libraryState . eventsState)
            me <- liftIO $ atomically $ tryReadTQueue tc
            case me of
                Just e -> fetchEvents' (e:xs)
                Nothing -> return xs

-- | Some events aren't meant to impact the user, and should be handled
-- internally by framework instead.
filterEventsForEndUser :: [Event] -> [Event]
filterEventsForEndUser = filter allowedEvent
    where
        allowedEvent :: Event -> Bool
        allowedEvent EventWindowClose         = True
        allowedEvent (EventWindowFocus _)     = True
        allowedEvent (EventMouseButton _ _ _) = True
        allowedEvent (EventCursorPos _ _)     = True
        allowedEvent (EventScroll _ _)        = True
        allowedEvent (EventKey _ _ _ _)       = True
        allowedEvent (EventChar _)            = True
        allowedEvent _ = False
