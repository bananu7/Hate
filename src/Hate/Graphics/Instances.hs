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
import Data.List
import Data.Ord

    {- |Drawing a mesh by itself doesn't make much sense; 
 - it has to have a pipeline prepared beforehand. -}
instance Drawable Mesh where
    draw m@(Mesh _ _ _) = drawMesh GL.TriangleFan m
    draw (IndexedMesh _vao _vbo _ibo _) = HateDraw $ liftIO $ do
        GL.bindVertexArrayObject $= Just _vao
        GL.bindBuffer GL.ArrayBuffer $= Just _vbo
        GL.bindBuffer GL.ElementArrayBuffer $= Just _ibo
        error "todo"
 
instance Drawable MeshWireframe where
    draw (MeshWireframe m) = drawMesh GL.LineLoop m

drawMesh drawingMode (Mesh _vao buffer n) = HateDraw $ liftIO $ do
    GL.bindVertexArrayObject $= Just _vao
    GL.bindBuffer GL.ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)

    GL.drawArrays drawingMode 0 (fromIntegral n `div` 2) 

instance Drawable Instance where 
    draw (Instance _mesh pip pos) = do 
        HateDraw $ liftIO $ do 
            let prog = program pip
            posLoc <- GL.get (GL.uniformLocation prog "instance_position")
 
            GL.currentProgram $= Just prog
            GL.uniform posLoc $= pos
        draw _mesh

instance Drawable Polygon where
    draw = singletonPolygonDraw

singletonPolygonDraw :: Polygon -> Action ()
singletonPolygonDraw (Polygon verts) = do
    m <- gets globalMesh
    fromVertArrayIntoGlobal rawVerts
    --fromVertArrayInto m rawTexCoords

    draw m

    where rawVerts = map realToFrac . concat . map unpackVec $ verts
          unpackVec (Vec2 x y) = [x, y]

          -- texturing-related computations
          maxX = _1 $ maximumBy (comparing _1) verts
          maxY = _2 $ maximumBy (comparing _2) verts
          scaleFactor = Vec2 (1 / maxX) (1 / maxY)

          texCoords = map (pointwise scaleFactor) verts
          rawTexCoords = map realToFrac . concat . map unpackVec $ texCoords


instance Drawable PolygonWireframe where
    draw (PolygonWireframe (Polygon verts)) = do
        mesh <- HateDraw $ liftIO $ fromVertArray rawVerts
        draw (MeshWireframe mesh)
        where rawVerts = map realToFrac . concat . map unpackVec $ verts
              unpackVec (Vec2 x y) = [x, y]

--instance Transformable Sprite where
--    transform t s = s { transformation = t }

instance Drawable Sprite where
    draw (Sprite (sx, sy) tex) = do
        HateDraw $ liftIO $ do
            GL.activeTexture $= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D $= Just tex
        let fsx = fromIntegral sx
            fsy = fromIntegral sy
        draw $ Polygon [Vec2 0 0, Vec2 fsx 0, Vec2 fsx fsy, Vec2 0 fsy]
