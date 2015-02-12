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
import qualified Data.ByteString.Char8 as BS (ByteString, unlines)
import Control.Monad.State

type ShaderSource = BS.ByteString

globalBufferSize = 1000

initialGraphicsState :: IO GraphicsState
initialGraphicsState =
    GraphicsState <$> createPipelineFromSources texturingPipelineSources
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

withGlobalPipeline :: HateDraw us () -> HateDraw us ()
withGlobalPipeline a = do
    gp <- gets mainPipeline
    withPipeline gp a


-- global, shared pipeline things
globalShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderSource
globalShader = shader Version450 MediumPrecision

globalVertexInputs :: [Input]
globalVertexInputs = [Input Vec2Tag (Just $ Location 0) "position"]

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
        vss = "    gl_Position = screen_transformation * vec4(position, 0, 1);"
        fss = "    color = vec4(0.8, 0.3, 0.3, 1.0);"

texturingPipelineSources :: (ShaderSource, ShaderSource)
texturingPipelineSources = makeGlobalPipelineSources 
    [] -- no additional vertex inputs
    [] -- no additional vertex uniforms
    [Varying Vec2Tag "var_position"] -- simple passthrough this time
    [Uniform Sampler2DTag (Just $ Binding 0) "mainTexture"] -- our sprite texture
    vss
    fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ,"    var_position = position / 10;"
            ]
        fss = "    color = vec4(texture(mainTexture, var_position).rgb, 1.0);"


--withTransformation :: Transformation -> Action () -> Action ()
--withTransformation t a = do

