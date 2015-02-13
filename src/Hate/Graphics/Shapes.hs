{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Shapes where

import Hate.Graphics.Types
import Hate.Math
import Hate.Math.Types

import Control.Monad.State

--line :: Vec2 -> Vec2 -> Action
--line a b = draw $ Polygon [a, b]

translate :: Vec2 -> DrawRequest -> DrawRequest
translate p d = d { transformation = newT }
    where oldT = transformation d
          oldP = position oldT
          newT = oldT { position = p + oldP }

scaled :: Vec2 -> DrawRequest -> DrawRequest
scaled s d = d { transformation = newT }
    where oldT = transformation d
          oldS = scale oldT
          newT = oldT { scale = s * oldS }

circle :: Float -> DrawRequest
circle r = DrawRequest verts FanVertexLayout Nothing identityTransform (SolidColorPipeline $ Vec4 1 0 0 1)
    where 
          verts = map (flip rotate $ vec2 0 r) $ angles
          angles = map ((* pi2) . (/ segNum)) $ [0..(segNum-1)] 
          pi2 = 2 * pi
          segNum = 100





