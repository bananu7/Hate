module Glisha.Math.Types where
	
import Data.Vect.Float
import Data.Vect.Float.Instances()

data Transformation = Transformation { 
    position :: Vec2,
    rotation :: Rotation,
    scale :: Vec2
    }

type Rotation = Float