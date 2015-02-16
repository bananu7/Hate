{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Util where

--import qualified Codec.Picture as JP
--import Data.Vector.Storable (unsafeWith)

import Hate.Graphics.Pipeline
import Hate.Graphics.Pipeline.Util
import Hate.Graphics.Types
import Hate.Graphics.Shader
import Hate.Math

import Control.Applicative

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Data.ByteString.Char8 as BS (ByteString)

initialGraphicsState :: IO GraphicsState
initialGraphicsState =
    GraphicsState <$> createPipelineFromSources solidColorPipelineSources
                  <*> createPipelineFromSources texturingPipelineSources
                  <*> createVertexStream

createVertexStream :: IO VertexStream
createVertexStream = do
    _vao <- (GL.genObjectName :: IO GL.VertexArrayObject)
    _vbo <- U.makeBuffer GL.ArrayBuffer ([] :: [Vec2])
    _texVbo <- U.makeBuffer GL.ArrayBuffer ([] :: [Vec2])
    let _vertNum = 0

    GL.bindVertexArrayObject $= Just _vao
    GL.bindBuffer GL.ArrayBuffer $= (Just _vbo)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)

    GL.bindBuffer GL.ArrayBuffer $= (Just _texVbo)
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)

    return $ VertexStream { vao = _vao, vbo = _vbo, texVbo = _texVbo, vertNum = _vertNum }

type ShaderSource = BS.ByteString

-- global, shared pipeline things
globalShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderSource
globalShader = shader Version450 MediumPrecision

globalVertexInputs :: [Input]
globalVertexInputs = 
    [ Input Vec2Tag (Just $ Location 0) "position"
    , Input Vec2Tag (Just $ Location 1) "texcoord"
    ]

globalVertexUniforms :: [Uniform]
globalVertexUniforms = [Uniform Mat4Tag Nothing "screen_transformation"]

globalVertexShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderSource
globalVertexShader i o u s = globalShader
    (globalVertexInputs ++ i)
    o
    (globalVertexUniforms ++ u)
    s

globalFragmentOutputs :: [Output]
globalFragmentOutputs = [Output Vec4Tag "color"]

globalFragmentUniforms :: [Uniform]
globalFragmentUniforms = [] -- TODO: add time

globalFragmentShader :: [Input] -> [Uniform] -> String -> ShaderSource
globalFragmentShader i u s = globalShader
    i
    globalFragmentOutputs
    (globalFragmentUniforms ++ u)
    s

createPipelineFromSources :: (ShaderSource, ShaderSource) -> IO Pipeline
createPipelineFromSources (vss,fss) = createPipelineSource vss fss

makeGlobalPipelineSources :: [Input] -> [Uniform] -> [Varying] -> [Uniform] -> String -> String -> (ShaderSource, ShaderSource)
makeGlobalPipelineSources vertexInputs vertexUniforms varyings fragmentUniforms vss fss =
    ( globalVertexShader vertexInputs (map toOutput varyings) vertexUniforms vss
    , globalFragmentShader (map toInput varyings) fragmentUniforms fss
    )

solidColorPipelineSources :: (ShaderSource, ShaderSource)
solidColorPipelineSources = makeGlobalPipelineSources [] [] [] [] vss fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ]
        
        fss = "    color = vec4(0.8, 0.3, 0.3, 1.0);"

texturingPipelineSources :: (ShaderSource, ShaderSource)
texturingPipelineSources = makeGlobalPipelineSources 
    [] -- no additional vertex inputs
    [] -- no additional vertex uniforms
    [ Varying Vec2Tag "var_position"
    , Varying Vec2Tag "var_texcoord"
    ]
    [Uniform Sampler2DTag (Just $ Binding 0) "mainTexture"] -- our sprite texture
    vss
    fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ,"    var_position = position / 10;"
            ,"    var_texcoord = texcoord;"
            ]
        fss = "    color = texture(mainTexture, var_texcoord);"


-- transformation-related thingies

--withTransformation :: Transformation -> Action () -> Action ()
--withTransformation t a = do

