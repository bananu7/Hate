module Hate.Graphics.Pipeline.Util where

import Hate.Graphics.Pipeline

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as U

import Data.Vect.Float
import Data.Vect.Float.OpenGL

import qualified Data.ByteString as BS

activatePipeline :: Pipeline -> IO ()
activatePipeline p = GL.currentProgram $= Just (program p)

--setUniformM3 :: Pipeline -> String -> Mat3 -> IO ()
--setUniformM3 = setUniformMatGeneric U.uniformGLMat4

setUniformM4 :: Pipeline -> String -> Mat4 -> IO ()
setUniformM4 = setUniformMatGeneric U.uniformGLMat4

setUniformMatGeneric setter pip name mat = do
    activatePipeline pip
    matLoc <- GL.get (GL.uniformLocation (program pip) name)

    let tMat = mat
    glmat <- makeGLMatrix tMat
    setter matLoc $= glmat

-- "deep" utils
createPipelineSource :: BS.ByteString -> BS.ByteString -> IO Pipeline
createPipelineSource vss fss = do 
    vs <- U.loadShaderBS "VertexShader" GL.VertexShader vss
    fs <- U.loadShaderBS "FragmentShader" GL.FragmentShader fss
    prog <- U.linkShaderProgram [vs, fs]

    return $ Pipeline vs fs prog

-- |This function takes paths to vertex and fragment shaders
createPipeline :: FilePath -> FilePath -> IO Pipeline
createPipeline vertShaderPath fragShaderPath = do
    vs <- BS.readFile vertShaderPath
    fs <- BS.readFile fragShaderPath
    createPipelineSource vs fs
