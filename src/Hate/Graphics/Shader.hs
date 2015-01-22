module Hate.Graphics.Shader
	( shader
	)
where

data Version = Version330
instance Show Version where
	show Version330 = "#version 330"

data FloatPrecision = HighPrecision | MediumPrecision | LowPrecision
instance Show FloatPrecision where
	show HighPrecision		-> "precision highp float;"
	show MediumPrecision 	-> "precision mediump float;"
	show LowPrecision 		-> "precision lowp float;"

data Input = 
	  InputFloat String
	| InputVec2 String
	| InputVec3 String
instance Show Input where
	show (InputFloat s) = "in float " ++ s ++ ";"
	show (InputVec2 s) = "in vec2 " ++ s ++ ";"
	show (InputVec3 s) = "in vec3 " ++ s ++ ";"

shader :: Version -> FloatPrecision -> [Input] -> String -> String
shader v p is body = header ++ "{" ++ body ++ "}"
	where header = unwords . map show $ [v, p] ++ is

passThroughVertexShader = 

solidColorFragmentShader = 

textureFragmentShader

