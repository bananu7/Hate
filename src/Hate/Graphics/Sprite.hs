module Hate.Graphics.Sprite where

import Hate.Graphics.Internal

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

loadImageDataIntoTexture (JP.ImageRGBA8 (JP.Image width height dat)) =
    -- Access the data vector pointer
    unsafeWith dat $ \ptr ->
        -- Generate the texture
        GL.texImage2D
        -- No cube map
        GL.Texture2D
        -- No proxy
        GL.NoProxy
        -- No mipmaps
        0
        -- Internal storage format: use R8G8B8A8 as internal storage
        GL.RGBA8
        -- Size of the image
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
        -- No borders
        0
        -- The pixel data: the vector contains Bytes, in RGBA order
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

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
sprite t = Sprite (vec2 1 1) t

--instance Drawable Sprite where
--    draw = 

--createSprite :: Texture -> Sprite


--instance Drawable Sprite where 
--    draw = 