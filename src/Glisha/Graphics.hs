{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glisha.Graphics where

{-|
Module      : Glisha.Graphics
Description : Hate graphics 
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental
Portability : To whatever has OpenGL in a reasonable version.

This module contains graphics
-}

import Glisha.Common
import Glisha.Math
import Glisha.Graphics.Pipeline
import Glisha.Graphics.Types
import Glisha.Graphics.Drawable.Class
import Glisha.Graphics.Instances

--import qualified Codec.Picture as JP
import Data.Vector.Storable (unsafeWith)

import Control.Applicative

{-
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D, Level, Border, TextureSize2D(..)) as GL
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (Proxy(..), PixelInternalFormat(..)) as GL
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (PixelData(..)) as GL
-}

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Data.ByteString.Char8 as BS (unlines)

fromVertArray :: [GL.GLfloat] -> IO Mesh
fromVertArray verts =
    Mesh <$> (GL.genObjectName :: IO GL.VertexArrayObject)
         <*> U.makeBuffer GL.ArrayBuffer verts
         <*> pure (length verts)

solidColorPipeline :: IO Pipeline
solidColorPipeline = createPipelineSource passtroughVsSource solidColorFsSource 

passtroughVsSource = BS.unlines $
    ["#version 330 core"
    ,""
    ,"layout(location = 0) in vec2 position;"
    --,"uniform vec2 instance_position;"
    ,""
    ,"uniform mat4 screen_transformation;"
    ,""
    ,"void main() {"
    ,"    gl_Position = screen_transformation * vec4(position, 0, 1);"
    ,"}"
    ]

solidColorFsSource = BS.unlines $
    ["#version 330 core"
    ,"out vec4 color;"
    ,"void main () {"
    ,"color = vec4(1.0, 1.0, 0.0, 1.0);"
    ,"}"
    ]

singletonPolygonDraw :: Polygon -> Glisha us ()
--todo: replace by a singleton passtrough streaming buffer setup
--It would require Glisha to be configurable(?) or simply adding it to it
singletonPolygonDraw (Polygon verts) = do
    mesh <- UnsafeGlisha $ liftIO $ fromVertArray rawVerts
    -- TODO
    --gets mainPipeline) >>= activatePipeline
    draw mesh

    where rawVerts = map realToFrac . concat . map unpackVec $ verts
          unpackVec (Vec2 x y) = [x, y]


--drawSquare t = draw $ Polygon $ transform t [vec 0 0, vec 0 1, vec 1 1, vec 1 0]


    {-
    | (ImageRGBA8 (Image width height dat)) =
  -- Access the data vector pointer
        unsafeWith dat $ \ ptr
            -- Generate the texture
            GL.texImage2D
            -- No cube map
            Nothing
            -- No proxy
            NoProxy
            -- No mipmaps
            0
            -- Internal storage format: use R8G8B8A8 as internal storage
            GL.RGBA8
            -- Size of the image
            (GL.TextureSize2D width height)
            -- No borders
            0
            -- The pixel data: the vector contains Bytes, in RGBA order
            (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    -}

{-
loadImageDataIntoTexture :: JP.DynamicImage -> IO ()
loadImageDataIntoTexture (JP.ImageRGB8 (JP.Image width height dat)) = 
    unsafeWith dat $ \ptr -> GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (GL.PixelData GL.RGB GL.UnsignedByte ptr)
loadImageDataIntoTexture _ = error "Not yet supported"

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture path = do
    image <- JP.readImage path
    case image of
        (Left err) -> do print err
                         exitWith (ExitFailure 1)
        (Right imgData) -> do texId <- GL.genObjectName :: IO GL.TextureObject
                              GL.textureBinding GL.Texture2D $= Just texId
                              loadImageDataIntoTexture imgData
                              return texId

sprite :: GL.TextureObject -> Sprite
sprite = Sprite identityTransform (vec2 1 1)


--instance Drawable Sprite where
--    draw = 

--createSprite :: Texture -> Sprite


--instance Drawable Sprite where 
--    draw = 

-}