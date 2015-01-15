module Glisha.Common.Instances where

import Control.Monad.State

instance MonadState us (Glisha us libs) where
get = UnsafeGlisha $ do
        gs <- get
        return $ userState gs

put s = UnsafeGlisha $ do
        gs <- get
        put $ gs { userState = s }