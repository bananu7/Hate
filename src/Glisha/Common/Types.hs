{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glisha.Common.Types where

import Control.Monad.State
import Control.Applicative
import qualified Graphics.UI.GLFW as G

-- TODO
type LibraryState = Int

{-| Configuration object to pass to `runApp` -}
data Config
    = Config
        { windowTitle :: String
        , windowSize :: (Int, Int)
        } deriving (Eq, Show)

-- GlishaInner is the inner Glisha state used by the API

data GlishaState us = GlishaState { 
  userState :: us,
  libraryState :: LibraryState,
  window :: G.Window,
  drawFn :: DrawFn us
}
type GlishaInner us a = StateT (GlishaState us) IO a

-- |Glisha Monad restricts user operations
newtype Glisha us a = UnsafeGlisha { runGlisha :: GlishaInner us a }
  deriving (Functor, Applicative, Monad, MonadIO)

{- |This is one of the two functions that the user has to
 - provide in order to use the framework. It's a regular IO
 - function, so it's not limited in any way. It has to produce
 - initial state of the user's program. -}
type LoadFn userStateType = IO userStateType
{- |The main framework update function runs in the restricted
 - Glisha context. Only safe Glisha functions can be used inside.
 - Because Glisha is an instance of MonadState, it can be treated
 - just as the State monad with the registered user data. -}
type DrawFn us = Glisha us () 
