{-# LANGUAGE OverloadedStrings #-}

module Hate.Graphics.Shader where

import qualified Data.ByteString.Char8 as BS (ByteString, pack)

type Name = String

data FloatPrecision = HighPrecision | MediumPrecision | LowPrecision

newtype Location = Location Int

data TypeTag = FloatTag | Vec2Tag | Vec3Tag | Vec4Tag | Mat2Tag | Mat3Tag | Mat4Tag | Sampler2DTag

data Input = Input TypeTag (Maybe Location) Name
    
data Output = Output TypeTag Name

newtype Binding = Binding Int

data Uniform = Uniform TypeTag (Maybe Binding) Name

data Varying = Varying TypeTag Name

toInput :: Varying -> Input
toInput (Varying tag name) = Input tag Nothing name

toOutput :: Varying -> Output
toOutput (Varying tag name) = Output tag name
