{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Hate.Common.Instances where

import Hate.Common.Types
import Control.Monad.State
import Control.Monad.Reader

instance MonadState us (Hate us) where
    get = UnsafeHate $ gets userState

    put s = UnsafeHate $ do
            gs <- get
            put $ gs { userState = s }

instance MonadReader us (HateDraw us) where
	ask = HateDraw $ gets userState