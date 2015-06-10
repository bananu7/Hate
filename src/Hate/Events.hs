module Hate.Events
    ( initialEventsState
    , setCallbacks
    , fetchEvents
    , allowedEvent
    , module Hate.Events.Types
    )
where

import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Hate.Events.Types
import Hate.Common.Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Control.Applicative
import Data.Maybe

import GHC.Float (double2Float)

initialEventsState :: IO EventsState
initialEventsState = newTQueueIO :: IO (TQueue TimedEvent)

{- The code has been borrowed from GLFW-b-demo; thanks @bsl -}

-- I assume only one window can be used by the framework

time = fromJust <$> GLFW.getTime

writeWithTime :: TQueue TimedEvent -> Event -> IO ()
writeWithTime tc e = time >>= \t -> atomically . writeTQueue tc $ (t, e)

errorCallback           :: TQueue TimedEvent -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue TimedEvent -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue TimedEvent -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue TimedEvent -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue TimedEvent -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue TimedEvent -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue TimedEvent -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue TimedEvent -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue TimedEvent -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue TimedEvent -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue TimedEvent -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue TimedEvent -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue TimedEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue TimedEvent -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = writeWithTime tc $ EventError           e s
windowPosCallback       tc _ x y          = writeWithTime tc $ EventWindowPos       x y
windowSizeCallback      tc _ w h          = writeWithTime tc $ EventWindowSize      w h
windowCloseCallback     tc _              = writeWithTime tc $ EventWindowClose
windowRefreshCallback   tc _              = writeWithTime tc $ EventWindowRefresh
windowFocusCallback     tc _ fa           = writeWithTime tc $ EventWindowFocus     fa
windowIconifyCallback   tc _ ia           = writeWithTime tc $ EventWindowIconify   ia
framebufferSizeCallback tc _ w h          = writeWithTime tc $ EventFramebufferSize w h
mouseButtonCallback     tc _ mb mba mk    = writeWithTime tc $ EventMouseButton     mb mba mk
cursorPosCallback       tc _ x y          = writeWithTime tc $ EventCursorPos       (double2Float x) (double2Float y)
cursorEnterCallback     tc _ ca           = writeWithTime tc $ EventCursorEnter     ca
scrollCallback          tc _ x y          = writeWithTime tc $ EventScroll          x y
keyCallback             tc _ k sc ka mk   = writeWithTime tc $ EventKey             k sc ka mk
charCallback            tc _ c            = writeWithTime tc $ EventChar            c

setErrorCallback :: TQueue TimedEvent -> IO ()
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

fetchEvents :: HateInner us [TimedEvent]
fetchEvents = fetchEvents' []
    where 
        fetchEvents' :: [TimedEvent] -> HateInner us [TimedEvent]
        fetchEvents' xs = do
            tc <- gets (eventsState . libraryState)
            me <- liftIO $ atomically $ tryReadTQueue tc
            case me of
                Just e -> fetchEvents' (e:xs)
                Nothing -> return xs

-- | Some events aren't meant to impact the user, and should be handled
-- internally by framework instead.

allowedEvent :: Event -> Bool
allowedEvent (EventWindowClose)       = True
allowedEvent (EventWindowFocus _)     = True
allowedEvent (EventMouseButton _ _ _) = True
allowedEvent (EventCursorPos _ _)     = True
allowedEvent (EventScroll _ _)        = True
allowedEvent (EventKey _ _ _ _)       = True
allowedEvent (EventChar _)            = True
allowedEvent _ = False
