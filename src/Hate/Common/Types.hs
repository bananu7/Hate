{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Hate.Common.Types where

import Control.Lens

import Control.Monad.State
import Control.Applicative
import Control.Concurrent.STM (TQueue)
import qualified Graphics.UI.GLFW as G

import Hate.Graphics.Types(DrawRequest)
import Hate.Graphics.Rendering
import Hate.Events.Types (Event)

data LibraryState = LibraryState {
    _GraphicsState :: RendererI,
    _EventsState :: TQueue Event
}
makeLenses ''LibraryState

-- |HateInner is the actual implementation backend
type HateInner us a = StateT (HateState us) IO a

-- |Hate Monad restricts user operations
newtype Hate us a = UnsafeHate { runHate :: HateInner us a }
    deriving (Functor, Applicative, Monad, MonadIO)


{- |This is one of the three functions that the user has to
provide in order to use the framework. It's a regular IO
function, so it's not limited in any way. It has to produce
initial state of the user's program. -}
type LoadFn userStateType = IO userStateType

type UpdateFn us = [Event] -> Hate us ()

type DrawFn us = us -> [DrawRequest]

{-| Configuration object to pass to `runApp` -}
data Config
    = Config
        { windowTitle :: String
        , windowSize :: (Int, Int)
        } deriving (Eq, Show)

data HateState us = HateState { 
    _UserState :: us,
    _LibraryState :: LibraryState,
    _Window :: G.Window,
    _DrawFn :: DrawFn us,
    _UpdateFn :: UpdateFn us,
    _LastUpdateTime :: Double
}
makeLenses ''HateState

