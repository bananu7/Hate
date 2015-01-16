{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Glisha.Common.Instances where

import Glisha.Common.Types
import Control.Monad.State

instance MonadState us (Glisha us) where
    get = UnsafeGlisha $ do
            gs <- get
            return $ userState gs

    put s = UnsafeGlisha $ do
            gs <- get
            put $ gs { userState = s }