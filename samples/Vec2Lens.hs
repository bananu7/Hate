module Vec2Lens(x,y) where

import Data.Vect.Float
import Control.Lens

x :: Simple Lens Vec2 Float
x = lens getX setX

y :: Simple Lens Vec2 Float
y = lens getY setY

getX :: Vec2 -> Float
getX (Vec2 x' _) = x'
getY :: Vec2 -> Float
getY (Vec2 _ y') = y'

setX :: Vec2 -> Float -> Vec2
setX (Vec2 _ y') nx = Vec2 nx y'
setY :: Vec2 -> Float -> Vec2
setY (Vec2 x' _) ny = Vec2 x' ny