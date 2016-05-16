module Hate.Math.Projection where

import Data.Vect.Float

-- | \"Orthogonal projecton\" matrix, a la OpenGL 
-- (the corresponding functionality is removed in OpenGL 3.1)
orthoMatrix 
  :: (Float,Float)   -- ^ (left,right)
  -> (Float,Float)   -- ^ (bottom,top)
  -> (Float,Float)   -- ^ (near,far)
  -> Mat4 
orthoMatrix (l,r) (b,t) (n,f) = Mat4
  (Vec4 (2/(r-l)) 0 0 0)
  (Vec4 0 (2/(t-b)) 0 0)
  (Vec4 0 0 (-2/(f-n)) 0)
  (Vec4 (-(r+l)/(r-l)) (-(t+b)/(t-b)) (-(f+n)/(f-n)) 1)
