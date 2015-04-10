{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Hate.Common.Instances() where

import Hate.Common.Types
import Control.Monad.State
import Control.Lens

instance MonadState us (Hate us) where
    get = UnsafeHate $ use userState
    put s = UnsafeHate $ userState .= s
    