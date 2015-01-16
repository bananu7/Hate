{-# LANGUAGE FlexibleInstances #-}

module Glisha.Math 
    ( module Data.Vect.Float
    , vec2
    , identityTransform
    , rotate
    )
where

import Data.Vect.Float
import Data.Vect.Float.Instances()

import Glisha.Math.Types

--type Vec2 = GL.Vertex2 Float
vec2 :: Float -> Float -> Vec2 
vec2 = Vec2

identityTransform :: Transformation
identityTransform = Transformation 0 0 1

rotate :: Rotation -> Vec2 -> Vec2
rotate a (Vec2 x y) = Vec2 (x * cos a - y * sin a) (x * sin a + y * cos a)
