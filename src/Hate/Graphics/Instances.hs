{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Instances where

import Hate.Common
import Hate.Math.Transformable.Class
import Hate.Graphics.Util
import Hate.Graphics.Internal
import Hate.Graphics.Drawable.Class
import Hate.Graphics.Pipeline
import Hate.Graphics.Types

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U

import Control.Monad.State
import Control.Monad.IO.Class

import Data.Vect.Float
    {- |Drawing a mesh by itself doesn't make much sense; 
 - it has to have a pipeline prepared beforehand. -}
instance Drawable Mesh where
    draw (Mesh _vao buffer n) = UnsafeHate $ liftIO $ do
        GL.bindVertexArrayObject $= Just _vao
        GL.bindBuffer GL.ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)
 
        GL.drawArrays GL.TriangleStrip 0 (fromIntegral n)
 
    draw (IndexedMesh _vao _vbo _ibo _) = UnsafeHate $ liftIO $ do
        GL.bindVertexArrayObject $= Just _vao
        GL.bindBuffer GL.ArrayBuffer $= Just _vbo
        GL.bindBuffer GL.ElementArrayBuffer $= Just _ibo
        error "todo"
 
instance Drawable MeshWireframe where
    draw (MeshWireframe (Mesh _vao buffer n)) = UnsafeHate $ liftIO $ do
        GL.bindVertexArrayObject $= Just _vao
        GL.bindBuffer GL.ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)
 
        GL.drawArrays GL.LineLoop 0 (fromIntegral n)

instance Drawable Instance where 
    draw (Instance _mesh pip pos) = do 
        UnsafeHate $ liftIO $ do 
            let prog = program pip
            posLoc <- GL.get (GL.uniformLocation prog "instance_position")
 
            GL.currentProgram $= Just prog
            GL.uniform posLoc $= pos
        draw _mesh

instance Drawable Polygon where
    draw = singletonPolygonDraw

singletonPolygonDraw :: Polygon -> Action ()
singletonPolygonDraw (Polygon verts) = do
    fromVertArrayIntoGlobal rawVerts
    m <- runHate2D $ gets globalMesh
    draw m

    where rawVerts = map realToFrac . concat . map unpackVec $ verts
          unpackVec (Vec2 x y) = [x, y]


instance Drawable PolygonWireframe where
    draw (PolygonWireframe (Polygon verts)) = do
        mesh <- UnsafeHate $ liftIO $ fromVertArray rawVerts
        draw (MeshWireframe mesh)
        where rawVerts = map realToFrac . concat . map unpackVec $ verts
              unpackVec (Vec2 x y) = [x, y]

instance Transformable Sprite where
    transform t s = s { transformation = t }