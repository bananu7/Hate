{-# LANGUAGE OverloadedStrings #-}

module Hate.Graphics.Shader
    ( shader,
      passThroughVertexShader
    )
where

import qualified Data.ByteString.Char8 as BS (pack)

data Version = Version330 | Version440 | Version450
instance Show Version where
    show Version330 = "#version 330"
    show Version440 = "#version 440"
    show Version450 = "#version 450"

data FloatPrecision = HighPrecision | MediumPrecision | LowPrecision
instance Show FloatPrecision where
    show HighPrecision      = "precision highp float;"
    show MediumPrecision    = "precision mediump float;"
    show LowPrecision       = "precision lowp float;"

newtype Location = Location Int
instance Show Location where 
    show (Location loc) =  "layout(location = " ++ show loc ++ ") "

data TypeTag = FloatTag | Vec2Tag | Vec3Tag
instance Show TypeTag where
    show FloatTag = "float"
    show Vec2Tag = "vec2"
    show Vec3Tag = "vec3"

data Input = Input TypeTag Location String
instance Show Input where
    show (Input tag loc name) = show loc ++ "in " ++ show tag ++ " " ++ show name ++ ";"
    
data Output = Output TypeTag String
instance Show Output where
    show (Output tag name) = "out " ++ show tag ++ " " ++ show name ++ ";"

shaderStr :: Version -> FloatPrecision -> [Input] -> [Output] -> String -> String
shaderStr v p ins outs body = header ++ "{\n" ++ body ++ "\n}\n"
    where versionStr = show v
          precisionStr = show p
          inputsStr = unlines . map show $ ins
          outputsStr = unlines . map show $ outs
          header = unlines [versionStr, precisionStr, inputsStr, outputsStr]
        
shader v p i o b = BS.pack $ shaderStr v p i o b

passThroughVertexShader = shader 
    Version330
    MediumPrecision
    [Input Vec2Tag (Location 0) "in_position"]
    [Output Vec2Tag "out_position"]
    "out_position = in_position;"
