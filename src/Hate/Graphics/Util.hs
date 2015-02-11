{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Util where

--import qualified Codec.Picture as JP
--import Data.Vector.Storable (unsafeWith)

import Hate.Common.Types
import Hate.Graphics.Pipeline
import Hate.Graphics.Pipeline.Util
import Hate.Graphics.Types
import Hate.Graphics.Drawable.Class
import Hate.Graphics.Internal
import Hate.Graphics.Shader

import Control.Applicative
import Control.Monad.IO.Class

{-
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D, Level, Border, TextureSize2D(..)) as GL
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (Proxy(..), PixelInternalFormat(..)) as GL
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (PixelData(..)) as GL
-}

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Data.ByteString.Char8 as BS (unlines)
import Control.Monad.State

globalBufferSize = 1000

initialGraphicsState :: IO GraphicsState
initialGraphicsState =
    GraphicsState <$> solidColorPipeline
                  <*> (fromVertArray $ replicate globalBufferSize 0)

fromVertArray :: [GL.GLfloat] -> IO Mesh
fromVertArray verts =
    Mesh <$> (GL.genObjectName :: IO GL.VertexArrayObject)
         <*> U.makeBuffer GL.ArrayBuffer verts
         <*> pure (length verts)

fromVertArrayInto :: [Float] -> Mesh -> Action Mesh
fromVertArrayInto verts m = HateDraw $ liftIO $ do
    GL.bindBuffer GL.ArrayBuffer $= Just (vbo m)
    U.replaceBuffer GL.ArrayBuffer verts
    return $ m { vertNum = length verts }

fromVertArrayIntoGlobal :: [Float] -> Action ()
fromVertArrayIntoGlobal xs = do
    m <- gets globalMesh
    m' <- fromVertArrayInto xs m
    modify $ \x -> x { globalMesh = m' }

solidColorPipeline :: IO Pipeline
solidColorPipeline = createPipelineSource passThroughVertexShader solidColorFsSource 

withGlobalPipeline :: HateDraw us () -> HateDraw us ()
withGlobalPipeline a = do
    gp <- gets mainPipeline
    withPipeline gp a

activateGlobalPipeline :: HateDraw us ()
activateGlobalPipeline = do
    gp <- gets mainPipeline
    activatePipeline gp

globalShader = shader Version450 MediumPrecision

passThroughVertexShader = globalShader 
    [Input Vec2Tag (Location 0) "position"]
    [Output Vec2Tag "fs_position"]
    [Uniform Mat4Tag (Location 0) "screen_transformation"]
    $ unlines
        ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
        ,"    fs_position = position / 10;"
        ]

{-
solidColorFsSource = globalShader
    [Input Vec2Tag "fs_position"]
    [Output Vec4Tag color]
    [Uniform Mat4Tag (Location 0) "screen_transformation"]
    $ unlines
        ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
        ,"    fs_position = position / 10;"
        ]
-}

solidColorFsSource = BS.unlines $
    ["#version 450 core"
    ,"out vec4 color;"
    ,"in vec2 fs_position;"
    ,"layout(binding = 0) uniform sampler2D mainTexture;"
    ,"void main () {"
    --,"      color = vec4(1.0, 1.0, 0.0, 1.0);"
    ,"      color = vec4(texture(mainTexture, fs_position).rgb, 1.0);"
    ,"}"
    ]

--withTransformation :: Transformation -> Action () -> Action ()
--withTransformation t a = do

