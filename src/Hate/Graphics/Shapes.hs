module Hate.Graphics.Shapes where

import Hate.Graphics.Types
import Hate.Math

--line :: Vec2 -> Vec2 -> Action
--line a b = draw $ Polygon [a, b]

translate :: Vec2 -> DrawRequest -> DrawRequest
translate p d = d { transformation = newT }
    where oldT = transformation d
          newT = oldT .*. positionToMatrix4 p

scaled :: Vec2 -> DrawRequest -> DrawRequest
scaled s d = d { transformation = newT }
    where oldT = transformation d
          newT = oldT .*. scaleToMatrix4 s

rotated :: Float -> DrawRequest -> DrawRequest
rotated r d = d { transformation = newT }
    where oldT = transformation d
          newT = oldT .*. rotationToMatrix4 r

circle :: Float -> DrawRequest
circle r = DrawRequest verts one FanVertexLayout Nothing one (SolidColorPipeline $ Vec4 1 0 0 1)
    where 
          verts = map (flip rotateVec $ vec2 0 r) $ angles
          angles = map ((* pi2) . (/ segNum)) $ [0..(segNum-1)] 
          pi2 = 2 * pi
          segNum = 100





