{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hate.Graphics.Internal where

import Hate.Graphics.Types
import Hate.Common.Types

import Control.Monad.State
import Control.Applicative

type Action a = forall us. HateDraw us a
