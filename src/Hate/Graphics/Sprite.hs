module Hate.Graphics.Sprite 
    ( loadSprite
    , sprite
    )
where

import Hate.Graphics.Types
import Hate.Math

import qualified Codec.Picture as JP
import Data.Vector.Storable (unsafeWith)

import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

--drawSquare t = draw $ Polygon $ transform t [vec 0 0, vec 0 1, vec 1 1, vec 1 0]

loadImageDataIntoTexture :: JP.DynamicImage -> IO ()

loadImageDataIntoTexture (JP.ImageRGB8 (JP.Image width height dat)) = 
    unsafeWith dat $ \ptr -> 
        GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.RGB8
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
        0
        (GL.PixelData GL.RGB GL.UnsignedByte ptr)

loadImageDataIntoTexture (JP.ImageRGBA8 (JP.Image width height dat)) = do
    unsafeWith dat $ GL.build2DMipmaps GL.Texture2D GL.RGBA8 (fromIntegral width) (fromIntegral height)
      . GL.PixelData GL.RGBA GL.UnsignedByte

loadImageDataIntoTexture _ = error "Not yet supported"

getImageSize :: JP.DynamicImage -> (Int, Int)
getImageSize (JP.ImageRGBA8 (JP.Image width height _)) = (width, height)

loadSprite :: FilePath -> IO Sprite
loadSprite path = do
    image <- JP.readImage path
    case image of
        (Left err) -> do print err
                         exitWith (ExitFailure 1)
        (Right imgData) -> do 
            texId <- GL.genObjectName :: IO GL.TextureObject
            GL.textureBinding GL.Texture2D $= Just texId
            loadImageDataIntoTexture imgData
            GL.textureFilter  GL.Texture2D GL.$= ((GL.Nearest, Just GL.Nearest), GL.Nearest)
            GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
            GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
            return $ Sprite { texture = texId, size = getImageSize imgData }

sprite :: Sprite -> DrawRequest
sprite (Sprite (w,h) t) = DrawRequest quad FanVertexLayout Nothing identityTransform (TexturingPipeline t)
    where quad = [Vec2 0 0, Vec2 fw 0, Vec2 fw fh, Vec2 0 fh]
          fw = fromIntegral w
          fh = fromIntegral h
