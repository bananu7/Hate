module Glisha.Graphics.Pipeline where

import Glisha.Common

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as U

import Data.Vect.Float.OpenGL (orthoMatrix, makeGLMatrix)
import Data.Vect.Float (transpose)

import qualified Data.ByteString as BS

-- |Pipeline object is a complete package needed to render something on the screen.
data Pipeline = Pipeline {
    vertexShader   :: GL.Shader,
    fragmentShader :: GL.Shader,
    program        :: GL.Program
    }

activatePipeline :: Pipeline -> Glisha us ()
activatePipeline p = UnsafeGlisha $ liftIO $ do 
    GL.currentProgram $= Just (program p)

createPipelineSource :: BS.ByteString -> BS.ByteString -> IO Pipeline
createPipelineSource vss fss = do 
    vs <- U.loadShaderBS "VertexShader" GL.VertexShader vss
    fs <- U.loadShaderBS "FragmentShader" GL.FragmentShader fss
    prog <- U.linkShaderProgram [vs, fs]

    matLoc <- GL.get (GL.uniformLocation prog "screen_transformation")
    GL.currentProgram $= Just prog

    let orthoScreenMat = orthoMatrix (-1, 2) (-1, 2) (-10, 10)
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
    
