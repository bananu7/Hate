{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Shapes where

import Hate.Graphics
import Hate.Graphics.Internal
import Hate.Math

import Control.Monad.State

data DrawMode = Filled | Outline deriving(Show, Eq)

--line :: Vec2 -> Vec2 -> Action
--line a b = draw $ Polygon [a, b]

circle :: DrawMode -> Vec2 -> Float -> Action ()
circle _ p r = draw $  Polygon verts
    where 
          verts = map ((+p) . (flip rotate $ vec2 0 r)) $ angles
          angles = map ((* pi2) . (/ segNum)) $ [0..(segNum-1)] 
          pi2 = 2 * pi
          segNum = 100

test :: Action ()
test = runHate2D $ do
    p <- liftIO solidColorPipeline
    modify $ \x -> x { mainPipeline = p }




