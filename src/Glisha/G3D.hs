{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Glisha.G3D
Description : 3D part of Glisha rendering features
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental
Portability : To whatever has OpenGL in a reasonable version.

This module is meant to be used mostly by 2D games and applications. Many
concepts and features have been deliberately simplified to make use easier;
if you need more control over the process, consider using the 3D counterpart.
-}

module Glisha.G3D where
    
-- control & data imports
import Control.Applicative((<$>), (<*>), pure)
 
-- GL & OS imports
import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as U (makeBuffer, offset0)

-- file imports
import Glisha.Common
import Glisha.Pipeline
 
-- this defines additional state used by that API
runApp = runAppInner (0 :: Int)

-- |A general type for a graphical mesh, either in indexed or raw form.
data Mesh =   Mesh { vao :: GL.VertexArrayObject,  vbo :: GL.BufferObject, vertNum :: Int }
            | IndexedMesh { vao :: GL.VertexArrayObject, vbo :: GL.BufferObject, ibo :: GL.BufferObject, vertNum :: Int }
 
-- Mesh holds a lightweight vbo reference, so it is ok to store it "by value"
{- |Instance object is a Mesh bundled with a pipeline that is to be used to render it, and
 - its position in the world coordinates -}
data Instance = Instance { 
    mesh :: Mesh, 
    pipeline :: Pipeline,
    position :: GL.Vertex2 GL.GLfloat
    }

test :: Glisha us Int Int
test = get
 
fromVertArray :: [GL.GLfloat] -> IO Mesh
fromVertArray verts =
    Mesh <$> (GL.genObjectName :: IO GL.VertexArrayObject)
         <*> U.makeBuffer GL.ArrayBuffer verts
         <*> pure (length verts)

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


