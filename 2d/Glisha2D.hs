{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Glisha2D where

-- control & data imports
import Data.List
import Control.Lens
import Control.Monad(unless, when)
import Control.Applicative((<$>), (<*>), pure)
import Control.Concurrent (threadDelay)
 
-- GL & OS imports
import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil as U
import Data.Vect.Float.OpenGL (orthoMatrix, makeGLMatrix)
import Data.Vect.Float (transpose)

-- file imports
import Util 
import GlishaCommon
  
-- actual stuff
data Pipeline = Pipeline {
    _vertexShader   :: GL.Shader,
    _fragmentShader :: GL.Shader,
    _program        :: GL.Program
    }
makeLenses ''Pipeline
 
data Mesh =   Mesh { _vao :: GL.VertexArrayObject,  _vbo :: GL.BufferObject, _vertNum :: Int }
            | IndexedMesh { _vao :: GL.VertexArrayObject, _vbo :: GL.BufferObject, _ibo :: GL.BufferObject, _vertNum :: Int }
makeLenses ''Mesh
 
-- Mesh holds a lightweight vbo reference, so it is ok to store it "by value"
data Instance = Instance { 
    _mesh :: Mesh, 
    _pipeline :: Pipeline,
    _position :: GL.Vertex2 GL.GLfloat
    }
makeLenses ''Instance
 
fromVertArray :: [GL.GLfloat] -> IO Mesh
fromVertArray verts = 
    Mesh <$> (GL.genObjectName :: IO GL.VertexArrayObject)
         <*> makeBuffer GL.ArrayBuffer verts
         <*> pure (length verts)
 
instance Drawable Mesh where
    draw (Mesh vao buffer n) = do
        GL.bindVertexArrayObject $= Just vao
        GL.bindBuffer GL.ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 offset0)
 
        GL.drawArrays GL.TriangleStrip 0 (fromIntegral n)
 
    draw (IndexedMesh vao vbo ibo n) = do
        GL.bindVertexArrayObject $= Just vao
        GL.bindBuffer GL.ArrayBuffer $= Just vbo
        GL.bindBuffer GL.ElementArrayBuffer $= Just ibo
        error "todo"
 
instance Drawable Instance where 
    draw (Instance mesh pip pos) = do 
        let prog = view program pip
        posLoc <- GL.get (GL.uniformLocation prog "instance_position")
 
        GL.currentProgram $= Just prog
        GL.uniform posLoc $= pos
        draw mesh
 
createPipeline :: FilePath -> FilePath -> IO Pipeline
createPipeline vertShaderPath fragShaderPath = do
    vs <- loadShader GL.VertexShader vertShaderPath
    fs <- loadShader GL.FragmentShader fragShaderPath
    prog <- linkShaderProgram [vs, fs]

    matLoc <- GL.get (GL.uniformLocation prog "global_projection")
    GL.currentProgram $= Just prog

    let orthoScreenMat = orthoMatrix (0, 800) (600, 0) (-10, 10)
        tMat = Data.Vect.Float.transpose orthoScreenMat 
    
    glmat <- makeGLMatrix tMat
    U.uniformGLMat4 matLoc $= glmat

    return $ Pipeline vs fs prog
 

