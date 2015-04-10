module Hate.Events.Types where

import Control.Concurrent.STM (TQueue)
import qualified Graphics.UI.GLFW as GLFW

type EventsState = TQueue Event
type Time = Double

data Event =
    EventError           !GLFW.Error !String !Time
  | EventWindowPos       !Int !Int !Time
  | EventWindowSize      !Int !Int !Time
  | EventWindowClose     !Time
  | EventWindowRefresh   !Time
  | EventWindowFocus     !GLFW.FocusState !Time
  | EventWindowIconify   !GLFW.IconifyState !Time
  | EventFramebufferSize !Int !Int !Time
  | EventMouseButton     !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys !Time
  | EventCursorPos       !Float !Float !Time
  | EventCursorEnter     !GLFW.CursorState !Time
  | EventScroll          !Double !Double !Time
  | EventKey             !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys !Time
  | EventChar            !Char !Time
  deriving Show