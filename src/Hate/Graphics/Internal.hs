{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hate.Graphics.Internal where

import Hate.Graphics.Types
import Hate.Common.Types

import Control.Monad.State
import Control.Applicative

newtype Hate2D us a = Hate2D { runHate2D :: Hate us a }
	deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState GraphicsState (Hate2D us) where
    get = Hate2D $ UnsafeHate $ graphicsState <$> gets libraryState
    put x = Hate2D $ UnsafeHate $ do
        g <- get
        let ls = libraryState g
        put $ g { libraryState = ls { graphicsState = x } }

type Action a = forall us. Hate us a
