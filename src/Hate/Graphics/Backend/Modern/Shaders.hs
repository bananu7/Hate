module Hate.Graphics.Backend.Modern.Shaders where

import Hate.Graphics.Shader
import Hate.Graphics.Pipeline
import Hate.Graphics.Pipeline.Util
import qualified Data.ByteString.Char8 as BS (ByteString, pack)

showMaybe :: Show a => Maybe a -> String
showMaybe (Just x) = show x
showMaybe Nothing = ""

instance Show FloatPrecision where
    show HighPrecision      = "precision highp float;"
    show MediumPrecision    = "precision mediump float;"
    show LowPrecision       = "precision lowp float;"

instance Show Location where 
    show (Location loc) =  "layout(location = " ++ show loc ++ ") "

instance Show TypeTag where
    show FloatTag = "float"
    show Vec2Tag = "vec2"
    show Vec3Tag = "vec3"
    show Vec4Tag = "vec4"
    show Mat2Tag = "mat2"
    show Mat3Tag = "mat3"
    show Mat4Tag = "mat4"
    show Sampler2DTag = "sampler2D"

instance Show Input where
    show (Input tag loc name) = showMaybe loc ++ "in " ++ show tag ++ " " ++ name ++ ";"

instance Show Output where
    show (Output tag name) = "out " ++ show tag ++ " " ++ name ++ ";"

instance Show Binding where 
    show (Binding bnd) =  "layout(binding = " ++ show bnd ++ ") "

instance Show Uniform where
    show (Uniform tag bnd name) = showMaybe bnd ++ "uniform " ++ show tag ++ " " ++ name ++ ";"

shaderStr :: ShaderDesc -> String
shaderStr (ShaderDesc p ins outs unifs body) = header ++ "void main() {\n" ++ body ++ "\n}\n"
    where precisionStr = show p
          inputsStr = unlines . map show $ ins
          outputsStr = unlines . map show $ outs
          uniformsStr = unlines . map show $ unifs
          header = unlines [precisionStr, inputsStr, outputsStr, uniformsStr]

shaderBStr :: ShaderDesc -> BS.ByteString
shaderBStr sd = BS.pack $ shaderStr sd

createPipeline :: (ShaderDesc, ShaderDesc) -> IO Pipeline
createPipeline (sdv, sdf) = createPipelineSource (shaderBStr sdv) (shaderBStr sdf)