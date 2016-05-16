{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Hate.Math
Description : Hate mathematical utilities
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : provisional
Portability : full

This module mostly works with primitives from @Data.Vect@,
adding some utilities useful in rendering and OpenGL
interoperability.

-}

module Hate.Math 
    ( module Data.Vect.Float
    , module Hate.Math.Util
    , module Hate.Math.Types
    , module Hate.Math.OpenGL
    , module Hate.Math.Projection
    )
where

import Data.Vect.Float

import Data.Vect.Float.Instances()

import Hate.Math.Types
import Hate.Math.Util
import Hate.Math.OpenGL
import Hate.Math.Projection