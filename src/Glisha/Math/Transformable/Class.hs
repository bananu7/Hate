module Glisha.Math.Transformable.Class where

import Glisha.Math.Types

class Transformable t where
    transform :: Transformation -> t -> t