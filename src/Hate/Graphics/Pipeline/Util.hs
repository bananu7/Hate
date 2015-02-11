module Hate.Graphics.Pipeline.Util where

import Hate.Common.Types
import Hate.Graphics.Pipeline

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as U

import Data.Vect.Float.OpenGL (orthoMatrix, makeGLMatrix)
import Data.Vect.Float (transpose)

import Control.Monad.IO.Class

import qualified Data.ByteString as BS

activatePipeline :: Pipeline -> HateDraw us ()
activatePipeline p = HateDraw $ liftIO $ do 
    GL.currentProgram $= Just (program p)

withPipeline :: Pipeline -> HateDraw us () -> HateDraw us ()
withPipeline p action = do
    currProgram <- HateDraw . liftIO $ GL.get GL.currentProgram
    HateDraw . liftIO $ GL.currentProgram $= Just (program p)
    action
    HateDraw . liftIO $ GL.currentProgram $= currProgram    

{-
setUniformM4 :: Pipeline -> Data.Vect.Float.Mat4 -> Action ()
setUniformM4 p m = HateDraw . liftIO . withPipeline p $ do
    let tMat = Data.Vect.Float.transpose m
    glmat <- makeGLMatrix tMat
    U.uniformGLMat4 matLoc $= glmat
-}

-- "deep" utils
createPipelineSource :: BS.ByteString -> BS.ByteString -> IO Pipeline
createPipelineSource vss fss = do 
    vs <- U.loadShaderBS "VertexShader" GL.VertexShader vss
    fs <- U.loadShaderBS "FragmentShader" GL.FragmentShader fss
    prog <- U.linkShaderProgram [vs, fs]

    matLoc <- GL.get (GL.uniformLocation prog "screen_transformation")
    GL.currentProgram $= Just prog

    let orthoScreenMat = orthoMatrix (0, 100) (0, 100) (-10, 10)
        tMat = Data.Vect.Float.transpose orthoScreenMat
    
    glmat <- makeGLMatrix tMat
    U.uniformGLMat4 matLoc $= glmat

    return $ Pipeline vs fs prog

-- |This function takes paths to vertex and fragment shaders
createPipeline :: FilePath -> FilePath -> IO Pipeline
createPipeline vertShaderPath fragShaderPath = do
    vs <- BS.readFile vertShaderPath
    fs <- BS.readFile fragShaderPath
    createPipelineSource vs fs
