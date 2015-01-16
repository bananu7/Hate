module Hate.Math.Transformable.Class where

import Hate.Math.Types

class Transformable t where
    transform :: Transformation -> t -> t