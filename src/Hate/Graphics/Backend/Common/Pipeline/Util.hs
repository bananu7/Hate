{-# LANGUAGE FlexibleContexts #-}

module Hate.Graphics.Backend.Common.Pipeline.Util
    ( activatePipeline
    , setUniformM4 
    , createPipelineSource   
    )
where

import Hate.Graphics.Backend.Common.Pipeline

import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GL.Core45 as GLRaw

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (unless)
import Foreign

import Data.Vect.Float

import qualified Data.ByteString as BS

activatePipeline :: Pipeline -> IO ()
activatePipeline p = GL.currentProgram $= Just (program p)

unUL :: GL.UniformLocation -> GLint
unUL = unsafeCoerce

uniformGLMat4 :: GL.UniformLocation -> GL.SettableStateVar (GL.GLmatrix GL.GLfloat)
uniformGLMat4 loc = GL.makeSettableStateVar aux
  where aux m = GL.withMatrix m $ \_ -> GLRaw.glUniformMatrix4fv loc' 1 1
        loc' = unUL loc

-- | @loadShaderBS fileName shaderType src@ loads a shader from source
-- code, @src@. The file name is used only for error reporting.
loadShaderBS :: FilePath -> GL.ShaderType -> BS.ByteString -> IO GL.Shader
loadShaderBS filePath st src = do
    shader <- GL.createShader st
    GL.shaderSourceBS shader $= src
    GL.compileShader shader
    --printError
    ok <- GL.get (GL.compileStatus shader)
    infoLog <- GL.get (GL.shaderInfoLog shader)
    unless (null infoLog || infoLog == "\NUL")
         (mapM_ putStrLn
                ["Shader info log for '" ++ filePath ++ "':", infoLog, ""])
    unless ok $ do
        GL.deleteObjectName shader
        ioError (userError "shader compilation failed")
    return shader

-- |Link shaders into a 'Program'.
linkShaderProgram :: [GL.Shader] -> IO GL.Program
linkShaderProgram shaders = linkShaderProgramWith shaders (const $ return ())

-- |Link shaders into a 'Program' with the given action performed
-- after attaching shaders, but before linking the program. This is
-- most commonly used to set the 'bindFragDataLocation' state
-- variable.
linkShaderProgramWith :: [GL.Shader] -> (GL.Program -> IO ()) -> IO GL.Program
linkShaderProgramWith shaders prelink = do p <- GL.createProgram
                                           mapM_ (GL.attachShader p) shaders
                                           prelink p
                                           GL.linkProgram p
                                           return p

makeGLMatrix m = GL.withNewMatrix GL.ColumnMajor (flip poke m . (castPtr :: Ptr GL.GLfloat -> Ptr Mat4)) 

--setUniformM3 :: Pipeline -> String -> Mat3 -> IO ()
--setUniformM3 = setUniformMatGeneric U.uniformGLMat4

setUniformM4 :: Pipeline -> String -> Mat4 -> IO ()
setUniformM4 = setUniformMatGeneric uniformGLMat4

setUniformMatGeneric setter pip name mat = do
    activatePipeline pip
    matLoc <- GL.get (GL.uniformLocation (program pip) name)

    let tMat = mat
    glmat <- makeGLMatrix tMat
    setter matLoc $= glmat

-- "deep" utils
createPipelineSource :: BS.ByteString -> BS.ByteString -> IO Pipeline
createPipelineSource vss fss = do 
    vs <- loadShaderBS "VertexShader" GL.VertexShader vss
    fs <- loadShaderBS "FragmentShader" GL.FragmentShader fss
    prog <- linkShaderProgram [vs, fs]

    return $ Pipeline vs fs prog

-- |This function takes paths to vertex and fragment shaders
createPipelineFromFiles :: FilePath -> FilePath -> IO Pipeline
createPipelineFromFiles vertShaderPath fragShaderPath = do
    vs <- BS.readFile vertShaderPath
    fs <- BS.readFile fragShaderPath
    createPipelineSource vs fs