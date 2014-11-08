{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Glisha.G2D
Description : 2D part of Glisha rendering features
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental
Portability : To whatever has OpenGL in a reasonable version.

This module is meant to be used mostly by 2D games and applications. Many
concepts and features have been deliberately simplified to make use easier;
if you need more control over the process, consider using the 3D counterpart.
-}

module Glisha.G2D where

import Glisha.Common

import qualified Codec.Picture as JP
import Control.Lens
import Data.Vector.Storable (unsafeWith)

import System.Exit
{-
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D, Level, Border, TextureSize2D(..)) as GL
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (Proxy(..), PixelInternalFormat(..)) as GL
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (PixelData(..)) as GL
-}

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as GLU

import Data.Vect.Float
import Data.Vect.Float.Instances()
--type Vec2 = GL.Vertex2 Float
vec2 :: Float -> Float -> Vec2 
vec2 = Vec2

type Rotation = Float

data Transformation = Transformation { 
    _position :: Vec2,
    _rotation :: Rotation,
    _scale :: Vec2
    }
makeLenses ''Transformation

identityTransform :: Transformation
identityTransform = Transformation 0 0 1

rotate :: Rotation -> Vec2 -> Vec2
rotate a (Vec2 x y) = Vec2 (x * cos a - y * sin a) (x * sin a + y * cos a)

class Transformable t where
    transform :: Transformation -> t -> t

instance Transformable [Vec2] where
    transform (Transformation pos rot scale) = map ((*scale) . (+pos) . (rotate rot))

singletonPolygonDraw :: Polygon -> Glisha us ()
--todo: replace by a singleton passtrough streaming buffer setup
--It would require Glisha to be configurable(?) or simply adding it to it
singletonPolygonDraw (Polygon verts) = UnsafeGlisha $ liftIO $ do
    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    vbo <- GLU.makeBuffer GL.ArrayBuffer verts
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= (Just vbo) -- (vertexBuffer buffer)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0) 
    GL.drawArrays GL.TriangleStrip 0 (fromIntegral $ length verts)

data Polygon = Polygon [Vec2]
instance Drawable Polygon where
    draw = singletonPolygonDraw               

--drawSquare t = draw $ Polygon $ transform t [vec 0 0, vec 0 1, vec 1 1, vec 1 0]

data Sprite = Sprite {
    _transformation :: Transformation,
    _size :: Vec2,
    _texture :: GL.TextureObject
    }
makeLenses ''Sprite

instance Transformable Sprite where
    transform t s = s & transformation .~ t

loadImageDataIntoTexture :: JP.DynamicImage -> IO ()
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

loadImageDataIntoTexture (JP.ImageRGB8 (JP.Image width height dat)) = 
    unsafeWith dat $ \ptr -> GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (GL.PixelData GL.RGB GL.UnsignedByte ptr)

type Path = String
loadTexture :: Path -> IO GL.TextureObject
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
