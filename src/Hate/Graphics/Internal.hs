{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hate.Graphics.Internal where

import Hate.Graphics.Types
import Hate.Common.Types

import Control.Monad.State
import Control.Applicative

instance MonadState GraphicsState (HateDraw us) where
    get = HateDraw $ graphicsState <$> gets libraryState
    put x = HateDraw $ do
        g <- get
        let ls = libraryState g
        put $ g { libraryState = ls { graphicsState = x } }

type Action a = forall us. HateDraw us a
