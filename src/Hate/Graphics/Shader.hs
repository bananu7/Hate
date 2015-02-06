module Hate.Graphics.Shader
    ( shader
    )
where

import qualified Data.ByteString.Char8 as BS (pack)

data Version = Version330
instance Show Version where
    show Version330 = "#version 330"
    show Version440 = "#version 440"
    show Version450 = "#version 450"

data FloatPrecision = HighPrecision | MediumPrecision | LowPrecision
instance Show FloatPrecision where
    show HighPrecision      -> "precision highp float;"
    show MediumPrecision    -> "precision mediump float;"
    show LowPrecision       -> "precision lowp float;"

data Input = 
      InputFloat String
    | InputVec2 String
    | InputVec3 String
instance Show Input where
    show (InputFloat s) = "in float " ++ s ++ ";"
    show (InputVec2 s) = "in vec2 " ++ s ++ ";"
    show (InputVec3 s) = "in vec3 " ++ s ++ ";"

data Output = 
      OutputFloat String
    | OutputVec2 String
    | OutputVec3 String
instance Show Input where
    show (OutputFloat s) = "out float " ++ s ++ ";"
    show (OutputVec2 s) = "out vec2 " ++ s ++ ";"
    show (OutputVec3 s) = "out vec3 " ++ s ++ ";"

type Location = Int
data PreboundInput = PreboundInput Location Input
instance Show PreboundInput where
    show (PreboundInput loc inp) = "layout(location = " ++ show loc ++ ") " ++ show inp

shaderStr :: Version -> FloatPrecision -> [Input] -> [Output] -> String -> String
shaderStr v p ins outs body = header ++ "{" ++ body ++ "}"
    where header = (unlines . map show [v, p]) ++ map show ins ++ map show outs

shader = BS.pack . shader

passThroughVertexShader = shader 
    Version330
    MediumPrecision
    [PreboundInput 0 (InputVec2 "in_position")]
    [OutputVec2 "out_position"]
    "out position = in_position;"
