{-# LANGUAGE OverloadedStrings #-}

module Hate.Graphics.Shader where

import qualified Data.ByteString.Char8 as BS (ByteString, pack)

type Name = String

data Version = Version330 | Version440 | Version450
instance Show Version where
    show Version330 = "#version 330 core"
    show Version440 = "#version 440 core"
    show Version450 = "#version 450 core"

data FloatPrecision = HighPrecision | MediumPrecision | LowPrecision
instance Show FloatPrecision where
    show HighPrecision      = "precision highp float;"
    show MediumPrecision    = "precision mediump float;"
    show LowPrecision       = "precision lowp float;"

newtype Location = Location Int
instance Show Location where 
    show (Location loc) =  "layout(location = " ++ show loc ++ ") "

data TypeTag = FloatTag | Vec2Tag | Vec3Tag | Vec4Tag | Mat2Tag | Mat3Tag | Mat4Tag
instance Show TypeTag where
    show FloatTag = "float"
    show Vec2Tag = "vec2"
    show Vec3Tag = "vec3"
    show Vec4Tag = "vec4"
    show Mat2Tag = "mat2"
    show Mat3Tag = "mat3"
    show Mat4Tag = "mat4"

data Input = Input TypeTag Location Name
instance Show Input where
    show (Input tag loc name) = show loc ++ "in " ++ show tag ++ " " ++ name ++ ";"
    
data Output = Output TypeTag Name
instance Show Output where
    show (Output tag name) = "out " ++ show tag ++ " " ++ name ++ ";"

data Uniform = Uniform TypeTag Location Name
instance Show Uniform where
    show (Uniform tag loc name) = show loc ++ "uniform " ++ show tag ++ " " ++ name ++ ";"

shaderStr :: 
    Version ->
    FloatPrecision ->
    [Input] ->
    [Output] ->
    [Uniform] ->
    String ->
    String
shaderStr v p ins outs unifs body = header ++ "void main() {\n" ++ body ++ "\n}\n"
    where versionStr = show v
          precisionStr = show p
          inputsStr = unlines . map show $ ins
          outputsStr = unlines . map show $ outs
          uniformsStr = unlines . map show $ unifs
          header = unlines [versionStr, precisionStr, inputsStr, outputsStr, uniformsStr]

shader :: 
    Version ->
    FloatPrecision ->
    [Input] ->
    [Output] ->
    [Uniform] ->
    String ->
    BS.ByteString
shader v p i o u b = BS.pack $ shaderStr v p i o u b
