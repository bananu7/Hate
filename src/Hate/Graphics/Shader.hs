{-# LANGUAGE OverloadedStrings #-}

module Hate.Graphics.Shader where

-- data
type Name = String

data FloatPrecision = HighPrecision | MediumPrecision | LowPrecision

newtype Location = Location Int

data TypeTag = FloatTag | Vec2Tag | Vec3Tag | Vec4Tag | Mat2Tag | Mat3Tag | Mat4Tag | Sampler2DTag

data Input = Input TypeTag (Maybe Location) Name
    
data Output = Output TypeTag Name

newtype Binding = Binding Int

data Uniform = Uniform TypeTag (Maybe Binding) Name

data Varying = Varying TypeTag Name

data ShaderDesc = ShaderDesc {
    sdFloatPrecision :: FloatPrecision,
    sdInputs :: [Input],
    sdOutputs :: [Output],
    sdUniforms :: [Uniform],
    sdBody :: String
}

-- show
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

-- Utils

toInput :: Varying -> Input
toInput (Varying tag name) = Input tag Nothing name

toOutput :: Varying -> Output
toOutput (Varying tag name) = Output tag name
