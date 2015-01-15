{-# LANGUAGE FlexibleInstances #-}

module Glisha.Math 
    ( module Data.Vect.Float --reexport
    , Transformation(..)
    , Transformable(..)
    , vec2
    , identityTransform
    , rotate
) where

import Data.Vect.Float
import Data.Vect.Float.Instances()
--type Vec2 = GL.Vertex2 Float
vec2 :: Float -> Float -> Vec2 
vec2 = Vec2

type Rotation = Float

data Transformation = Transformation { 
    position :: Vec2,
    rotation :: Rotation,
    scale :: Vec2
    }

identityTransform :: Transformation
identityTransform = Transformation 0 0 1

rotate :: Rotation -> Vec2 -> Vec2
rotate a (Vec2 x y) = Vec2 (x * cos a - y * sin a) (x * sin a + y * cos a)

class Transformable t where
    transform :: Transformation -> t -> t

instance Transformable [Vec2] where
    transform (Transformation pos rot scal) = map ((*scal) . (+pos) . (rotate rot))
