{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Hate.Math.Instances where

import Hate.Math.Transformable.Class
import Hate.Math.Types
import Hate.Math.Util
import Data.Vect.Float

--instance Transformable [Vec2] where
--    transform (Transformation pos rot scal) = map ((*scal) . (+pos) . (rotate rot))
