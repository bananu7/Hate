module Hate.Events.Types where

import Control.Concurrent.STM (TQueue)
import qualified Graphics.UI.GLFW as GLFW

type EventsState = TQueue Event

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !Int !Int
  | EventWindowSize      !Int !Int
  | EventWindowClose
  | EventWindowRefresh
  | EventWindowFocus     !GLFW.FocusState
  | EventWindowIconify   !GLFW.IconifyState
  | EventFramebufferSize !Int !Int
  | EventMouseButton     !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !Float !Float
  | EventCursorEnter     !GLFW.CursorState
  | EventScroll          !Double !Double
  | EventKey             !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !Char
  deriving Show