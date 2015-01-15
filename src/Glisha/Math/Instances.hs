module Glisha.Math.Instances where

instance Transformable [Vec2] where
    transform (Transformation pos rot scal) = map ((*scal) . (+pos) . (rotate rot))