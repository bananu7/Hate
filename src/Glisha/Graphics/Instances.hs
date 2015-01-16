module Glisha.Graphics.Instances where

import Glisha.Common
import Glisha.Math.Transformable.Class
import Glisha.Graphics.Util
import Glisha.Graphics.Drawable.Class
import Glisha.Graphics.Pipeline
import Glisha.Graphics.Types

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U

import Control.Monad.IO.Class
import Data.Vect.Float
    {- |Drawing a mesh by itself doesn't make much sense; 
 - it has to have a pipeline prepared beforehand. -}
instance Drawable Mesh where
    draw (Mesh _vao buffer n) = UnsafeGlisha $ liftIO $ do
        GL.bindVertexArrayObject $= Just _vao
        GL.bindBuffer GL.ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)
 
        GL.drawArrays GL.TriangleStrip 0 (fromIntegral n)
 
    draw (IndexedMesh _vao _vbo _ibo _) = UnsafeGlisha $ liftIO $ do
        GL.bindVertexArrayObject $= Just _vao
        GL.bindBuffer GL.ArrayBuffer $= Just _vbo
        GL.bindBuffer GL.ElementArrayBuffer $= Just _ibo
        error "todo"
 
instance Drawable Instance where 
    draw (Instance _mesh pip pos) = do 
        UnsafeGlisha $ liftIO $ do 
            let prog = program pip
            posLoc <- GL.get (GL.uniformLocation prog "instance_position")
 
            GL.currentProgram $= Just prog
            GL.uniform posLoc $= pos
        draw _mesh

instance Drawable Polygon where
    draw = singletonPolygonDraw

singletonPolygonDraw :: Polygon -> Glisha us ()
--todo: replace by a singleton passtrough streaming buffer setup
--It would require Glisha to be configurable(?) or simply adding it to it
singletonPolygonDraw (Polygon verts) = do
    mesh <- UnsafeGlisha $ liftIO $ fromVertArray rawVerts
    -- TODO
    --gets mainPipeline) >>= activatePipeline
    draw mesh
    where rawVerts = map realToFrac . concat . map unpackVec $ verts
          unpackVec (Vec2 x y) = [x, y]


instance Transformable Sprite where
    transform t s = s { transformation = t }