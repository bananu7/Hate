module Hate.Graphics.Shapes where

import Hate.Graphics.Types
import Hate.Math

-- |Moves the drawn primitive. Note that order of operations matters.
translate :: Vec2 -> DrawRequest -> DrawRequest
translate p d = d { transformation = newT }
    where oldT = transformation d
          newT = oldT .*. positionToMatrix4 p

-- |Scales the drawn primitive.
scaled :: Vec2 -> DrawRequest -> DrawRequest
scaled s d = d { transformation = newT }
    where oldT = transformation d
          newT = oldT .*. scaleToMatrix4 s

-- |Rotates the drawn primitive in the Z axis (keeps it flat).
rotated :: Float -> DrawRequest -> DrawRequest
rotated r d = d { transformation = newT }
    where oldT = transformation d
          newT = oldT .*. rotationToMatrix4 r

colored :: Color -> DrawRequest -> DrawRequest
colored color (DrawRequest verts coords origin layout transf _) = DrawRequest verts coords origin layout transf (SolidColorPipeline color)

-- |Constructs a 'DrawRequest' containing vertices in a shape of a circle.
-- /Note that the current implementation always produces a fixed amount of vertices./
circle :: Float -> DrawRequest
circle r = DrawRequest verts Nothing one FanVertexLayout one (SolidColorPipeline (Vec4 1.0 1.0 1.0 1.0))
    where
        verts = map (flip rotateVec $ vec2 0 r) $ angles
        angles = map ((* pi2) . (/ segNum)) $ [0..(segNum-1)]
        pi2 = 2 * pi
        segNum = 100

line :: Vec2 -> Vec2 -> DrawRequest
line start end = DrawRequest [start, end] Nothing one LinesVertexLayout one (SolidColorPipeline (Vec4 1.0 1.0 1.0 1.0))
