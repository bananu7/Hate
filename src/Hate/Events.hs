module Hate.Events
    ( initialEventsState
    , setCallbacks
    , fetchEvents
    , module Hate.Events.Types
    )
where

import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Hate.Events.Types
import Hate.Common.Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)

initialEventsState :: IO EventsState
initialEventsState = newTQueueIO :: IO (TQueue Event)

{- The code has been borrowed from GLFW-b-demo; thanks @bsl -}

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
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            c

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
            tc <- gets (eventsState . libraryState)
            me <- liftIO $ atomically $ tryReadTQueue tc
            case me of
                Just e -> fetchEvents' (e:xs)
                Nothing -> return xs