{-# LANGUAGE RankNTypes #-}
module Glisha.Api where

-- Draft of the high-level API

import Glisha.G2D
import Glisha.Common
import Glisha.Math

data DrawMode = Filled | Outline deriving(Show, Eq)
type Action = forall us. forall libs. Glisha us libs ()

line :: Vec2 -> Vec2 -> Action
line a b = draw $ Polygon [a, b]

circle :: DrawMode -> Vec2 -> Float -> Action
circle _ p r = draw $ Polygon verts 
    where 
          verts = map ((+p) . (flip rotate $ vec2 0 r)) $ angles
          angles = map ((* pi2) . (/ segNum)) $ [0..(segNum-1)] 
          pi2 = 2 * pi
          segNum = r
 
