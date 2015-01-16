{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hate.Common.Types where

import Control.Monad.State
import Control.Applicative
import qualified Graphics.UI.GLFW as G

import Hate.Graphics.Types(GraphicsState)

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
  drawFn :: DrawFn us
}
type HateInner us a = StateT (HateState us) IO a

-- |Hate Monad restricts user operations
newtype Hate us a = UnsafeHate { runHate :: HateInner us a }
  deriving (Functor, Applicative, Monad, MonadIO)

{- |This is one of the two functions that the user has to
 - provide in order to use the framework. It's a regular IO
 - function, so it's not limited in any way. It has to produce
 - initial state of the user's program. -}
type LoadFn userStateType = IO userStateType
{- |The main framework update function runs in the restricted
 - Hate context. Only safe Hate functions can be used inside.
 - Because Hate is an instance of MonadState, it can be treated
 - just as the State monad with the registered user data. -}
type DrawFn us = Hate us () 
