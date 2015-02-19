{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hate.Common.Types
    ( Config(..)
    , LibraryState(..)
    , HateState(..)
    , HateInner
    , Hate(..)
    , HateDraw(..)
    , LoadFn
    , UpdateFn
    , DrawFn
    )
where

import Control.Monad.State
import Control.Applicative
import qualified Graphics.UI.GLFW as G

import Hate.Graphics.Types(GraphicsState, DrawRequest)

-- TODO
data LibraryState = LibraryState {
		graphicsState :: GraphicsState
	}

{-| Configuration object to pass to `runApp` -}
data Config
    = Config
        { windowTitle :: String
        , windowSize :: (Int, Int)
        } deriving (Eq, Show)

-- HateInner is the inner Hate state used by the API

data HateState us = HateState { 
    userState :: us,
    libraryState :: LibraryState,
    window :: G.Window,
    drawFn :: DrawFn us,
    updateFn :: UpdateFn us,
    lastUpdateTime :: Double
}
type HateInner us a = StateT (HateState us) IO a

-- |Hate Monad restricts user operations
newtype Hate us a = UnsafeHate { runHate :: HateInner us a }
    deriving (Functor, Applicative, Monad, MonadIO)

newtype HateDraw us a = HateDraw { runHateDraw :: HateInner us a }
  deriving (Functor, Applicative, Monad, MonadIO)

{- |This is one of the three functions that the user has to
 - provide in order to use the framework. It's a regular IO
 - function, so it's not limited in any way. It has to produce
 - initial state of the user's program. -}
type LoadFn userStateType = IO userStateType

type UpdateFn us = Hate us ()

type DrawFn us = us -> [DrawRequest]
