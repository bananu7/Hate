module Hate.Graphics.Backend.Compat.Shaders
    ( createPipelineNoUniformBindings
    )
where

import Hate.Graphics.Shader
import Hate.Graphics.Pipeline
import Hate.Graphics.Pipeline.Util
import qualified Data.ByteString.Char8 as BS (ByteString, pack)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Data.Maybe (catMaybes)

shaderStr :: ShaderDesc -> String
shaderStr (ShaderDesc p ins outs unifs body) = header ++ "void main() {\n" ++ body ++ "\n}\n"
    where versionStr = "#version 330 core"
          precisionStr = show p
          inputsStr = unlines . map show $ ins
          outputsStr = unlines . map show $ outs
          uniformsStr = unlines . map show $ unifs
          header = unlines [versionStr, precisionStr, inputsStr, outputsStr, uniformsStr]

shaderBStr :: ShaderDesc -> BS.ByteString
shaderBStr sd = BS.pack $ shaderStr sd

-- |This is a workaround for pre-4.2 OpenGL
createPipelineNoUniformBindings :: (ShaderDesc, ShaderDesc) -> IO Pipeline
createPipelineNoUniformBindings (sdv, sdf) = do
    let (bindsV, sdv') = stripUniformBindings sdv
    let (bindsF, sdf') = stripUniformBindings sdf

    p <- createPipelineSource (shaderBStr sdv') (shaderBStr sdf')
    mapM_ (applyPendingUniformBinding p) (bindsV ++ bindsF) 
    return p

data PendingUniformBinding = PendingUniformBinding String GL.TextureUnit

stripUniformBindings :: ShaderDesc -> ([PendingUniformBinding], ShaderDesc)
stripUniformBindings sd = (,) (catMaybes . map uniformToPendingUniform . sdUniforms $ sd) (strip sd)
    where strip sd = sd { sdUniforms = map strip1 $ sdUniforms sd }
          strip1 (Uniform typeTag _ name) = Uniform typeTag Nothing name

uniformToPendingUniform :: Uniform -> Maybe PendingUniformBinding
uniformToPendingUniform (Uniform typeTag maybeBinding name) = 
    fmap (\(Binding x) -> PendingUniformBinding name (GL.TextureUnit (fromIntegral x))) maybeBinding

applyPendingUniformBinding :: Pipeline -> PendingUniformBinding -> IO ()
applyPendingUniformBinding p (PendingUniformBinding name x) = do
    loc <- GL.get $ GL.uniformLocation (program p) name
    GL.uniform loc $= x
