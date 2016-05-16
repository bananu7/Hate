module Hate.Graphics.Backend.Modern.Shaders
	( createPipeline
	)
where

import Hate.Graphics.Shader
import Hate.Graphics.Backend.Common.Pipeline
import Hate.Graphics.Backend.Common.Pipeline.Util
import qualified Data.ByteString.Char8 as BS (ByteString, pack)

shaderStr :: ShaderDesc -> String
shaderStr (ShaderDesc p ins outs unifs body) = header ++ "void main() {\n" ++ body ++ "\n}\n"
    where versionStr = "#version 440 core"
          precisionStr = show p
          inputsStr = unlines . map show $ ins
          outputsStr = unlines . map show $ outs
          uniformsStr = unlines . map show $ unifs
          header = unlines [versionStr, precisionStr, inputsStr, outputsStr, uniformsStr]

shaderBStr :: ShaderDesc -> BS.ByteString
shaderBStr sd = BS.pack $ shaderStr sd

createPipeline :: (ShaderDesc, ShaderDesc) -> IO Pipeline
createPipeline (sdv, sdf) = createPipelineSource (shaderBStr sdv) (shaderBStr sdf)