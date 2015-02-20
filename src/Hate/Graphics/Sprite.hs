module Hate.Graphics.Sprite 
    ( loadSprite
    , sprite
    , spriteSheet
    )
where

import Hate.Graphics.Types
import Hate.Math

import qualified Codec.Picture as JP
import Data.Vector.Storable (unsafeWith)

import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import qualified Data.Map as Map

--drawSquare t = draw $ Polygon $ transform t [vec 0 0, vec 0 1, vec 1 1, vec 1 0]

loadImageDataIntoTexture :: JP.DynamicImage -> IO ()

loadImageDataIntoTexture (JP.ImageRGB8 (JP.Image width height dat)) = 
    unsafeWith dat $ GL.build2DMipmaps GL.Texture2D GL.RGB8 (fromIntegral width) (fromIntegral height)
      . GL.PixelData GL.RGB GL.UnsignedByte

loadImageDataIntoTexture (JP.ImageRGBA8 (JP.Image width height dat)) = do
    unsafeWith dat $ GL.build2DMipmaps GL.Texture2D GL.RGBA8 (fromIntegral width) (fromIntegral height)
      . GL.PixelData GL.RGBA GL.UnsignedByte

loadImageDataIntoTexture _ = error "Not yet supported"

getImageSize :: JP.DynamicImage -> (Int, Int)
getImageSize (JP.ImageRGBA8 (JP.Image width height _)) = (width, height)
getImageSize (JP.ImageRGB8 (JP.Image width height _)) = (width, height)

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

-- |Creates a 'DrawRequest' that draws a sprite. The 'OriginReference' parameter specifices the
-- "hooking point" for the rotations and translations.
sprite :: OriginReference -> Sprite -> DrawRequest
sprite originRef (Sprite (w,h) t) = DrawRequest quad Nothing originMat FanVertexLayout one (TexturingPipeline t)
    where
        quad = [Vec2 0 0, Vec2 fw 0, Vec2 fw fh, Vec2 0 fh]
        fw = fromIntegral w
        fh = fromIntegral h
        originMat = case originRef of 
            TopLeft -> one
            Middle -> positionToMatrix4 $ Vec2 (-fw/2) (-fh/2)


-- Regular sprite sheet specifies in how many parts should the file be cut horizontally and vertically
type SpriteSheet = (Int, Int)

data SpriteAtlasEntry = SpriteAtlasEntry { start :: Vec2, spriteSize :: Vec2 }
newtype SpriteAtlas = IrregularSpriteSheet (Map.Map String SpriteAtlasEntry)


spriteSheet :: (Int, Int) -> SpriteSheet -> Sprite -> DrawRequest
spriteSheet (coordX, coordY) (sx, sy) (Sprite (w,h) t) = DrawRequest quad texCoords one FanVertexLayout one (TexturingPipeline t)
    where 
        quad = [Vec2 0 0, Vec2 fw 0, Vec2 fw fh, Vec2 0 fh]
        texCoords = Just $ [ Vec2 txStart tyStart
                           , Vec2 txEnd tyStart
                           , Vec2 txEnd tyEnd
                           , Vec2 txStart tyEnd
                           ]

        txStart = fromIntegral coordX / fromIntegral sx
        tyStart = fromIntegral (coordY + 1) / fromIntegral sy
        txEnd = fromIntegral (coordX + 1) / fromIntegral sx
        tyEnd = fromIntegral coordY / fromIntegral sy

        fw = fromIntegral w / fromIntegral sx
        fh = fromIntegral h / fromIntegral sy
