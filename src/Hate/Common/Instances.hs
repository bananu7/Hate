{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Hate.Common.Instances where

import Hate.Common.Types
import Control.Monad.State

instance MonadState us (Hate us) where
    get = UnsafeHate $ do
            gs <- get
            return $ userState gs

    put s = UnsafeHate $ do
            gs <- get
            put $ gs { userState = s }