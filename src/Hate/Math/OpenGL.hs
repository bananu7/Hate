module Hate.Math.OpenGL where

import Data.Vect.Float
import qualified Graphics.Rendering.OpenGL as GL

toOpenGLVertex :: Vec4 -> GL.Vertex4 Float
toOpenGLVertex (Vec4 x y z w) = GL.Vertex4 x y z w