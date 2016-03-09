{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Backend.Util where

--import qualified Codec.Picture as JP
--import Data.Vector.Storable (unsafeWith)

import Hate.Graphics.Types
import Hate.Graphics.Shader
import Hate.Math

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Data.ByteString.Char8 as BS (ByteString)

import Data.List (maximumBy)
import Data.Ord
import Data.IORef

modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO ref mut = readIORef ref >>= mut >>= writeIORef ref

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

calculateTexCoords :: [Vec2] -> [Vec2]
calculateTexCoords verts = map (flipY . pointwise scaleFactor) verts
    where
        maxX = _1 $ maximumBy (comparing _1) verts
        maxY = _2 $ maximumBy (comparing _2) verts
        scaleFactor = Vec2 (1 / maxX) (1 / maxY)
        flipY (Vec2 x y) = Vec2 x y

vertexLayoutToGLLayout :: VertexLayout -> GL.PrimitiveMode
vertexLayoutToGLLayout FanVertexLayout = GL.TriangleFan
vertexLayoutToGLLayout StripVertexLayout = GL.TriangleStrip
vertexLayoutToGLLayout LinesVertexLayout = GL.Lines

type ShaderSource = BS.ByteString

-- global, shared pipeline things
globalShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderDesc
globalShader = ShaderDesc MediumPrecision

globalVertexInputs :: [Input]
globalVertexInputs =
    [ Input Vec2Tag (Just $ Location 0) "position"
    , Input Vec2Tag (Just $ Location 1) "texcoord"
    ]

globalVertexUniforms :: [Uniform]
globalVertexUniforms = [Uniform Mat4Tag Nothing "screen_transformation"]

globalVertexShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderDesc
globalVertexShader i o u s = globalShader
    (globalVertexInputs ++ i)
    o
    (globalVertexUniforms ++ u)
    s

globalFragmentOutputs :: [Output]
globalFragmentOutputs = [Output Vec4Tag "color"]

globalFragmentUniforms :: [Uniform]
globalFragmentUniforms = [] -- TODO: add time

globalFragmentShader :: [Input] -> [Uniform] -> String -> ShaderDesc
globalFragmentShader i u s = globalShader
    i
    globalFragmentOutputs
    (globalFragmentUniforms ++ u)
    s

makeGlobalPipelineDescs :: [Input] -> [Uniform] -> [Varying] -> [Uniform] -> String -> String -> (ShaderDesc, ShaderDesc)
makeGlobalPipelineDescs vertexInputs vertexUniforms varyings fragmentUniforms vss fss =
    ( globalVertexShader vertexInputs (map toOutput varyings) vertexUniforms vss
    , globalFragmentShader (map toInput varyings) fragmentUniforms fss
    )

solidColorPipelineDescs :: (ShaderDesc, ShaderDesc)
solidColorPipelineDescs = makeGlobalPipelineDescs [] [] []
    [Uniform Vec4Tag Nothing "in_color"]
    vss fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ]

        fss = "    color = in_color;"

texturingPipelineDescs :: (ShaderDesc, ShaderDesc)
texturingPipelineDescs = makeGlobalPipelineDescs
    [] -- no additional vertex inputs
    [] -- no additional vertex uniforms
    [ Varying Vec2Tag "var_position"
    , Varying Vec2Tag "var_texcoord"
    ]
    [Uniform Sampler2DTag Nothing "mainTexture"] -- our sprite texture
    vss
    fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ,"    var_position = position / 10;"
            ,"    var_texcoord = texcoord;"
            ]
        fss = "    color = texture(mainTexture, var_texcoord);"
