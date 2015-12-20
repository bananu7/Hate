module Hate.Graphics.Sprite 
    ( loadSprite
    , loadSpriteSheet
    , sprite
    , spriteSheet
    , spritePart
    )
where

import Hate.Graphics.Types
import Hate.Math

import qualified Codec.Picture as JP
import qualified Codec.Picture.RGBA8 as JPU
import Data.Vector.Storable (unsafeWith)

import System.Exit
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Control.Applicative

--drawSquare t = draw $ Polygon $ transform t [vec 0 0, vec 0 1, vec 1 1, vec 1 0]

loadImageDataIntoTexture :: JP.Image JP.PixelRGBA8 -> IO ()
loadImageDataIntoTexture (JP.Image width height dat) = do
    unsafeWith dat $ GL.build2DMipmaps GL.Texture2D GL.RGBA8 (fromIntegral width) (fromIntegral height)
      . GL.PixelData GL.RGBA GL.UnsignedByte

loadImageDataIntoTexture _ = error "Not yet supported"

getImageSize :: JP.Image JP.PixelRGBA8 -> (Int, Int)
getImageSize (JP.Image width height _) = (width, height)

-- |Loads a file from disk and constructs a drawable sprite.
loadSprite :: FilePath -> IO Sprite
loadSprite path = do
    image <- (fmap . fmap) JPU.fromDynamicImage $ JP.readImage path
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

-- |Loads a sprite sheet from disk.
loadSpriteSheet :: FilePath
                -> (Int, Int) -- ^ The size in tiles of the sheet
                -> IO SpriteSheet
loadSpriteSheet path sz = SpriteSheet <$> loadSprite path <*> pure sz

-- |Creates a 'DrawRequest' that draws a sprite. The 'OriginReference' parameter specifices the
-- "hooking point" for the rotations and translations.
sprite :: OriginReference -> Sprite -> DrawRequest
sprite originRef (Sprite (w,h) t) = DrawRequest quad Nothing originMat FanVertexLayout one (TexturingPipeline t)
    where
        quad = [Vec2 0 0, Vec2 fw 0, Vec2 fw fh, Vec2 0 fh]
        fw = fromIntegral w
        fh = fromIntegral h
        originMat = calcSpriteOriginMatrix originRef fw fh

-- |Creates a 'DrawRequest' with a rectangle cut out of a regular sprite sheet. The number specifies the 
-- index of the sprite, counting from the top-left one.
spriteSheet :: OriginReference -> Int -> SpriteSheet -> DrawRequest
spriteSheet originRef num (SpriteSheet (Sprite (w,h) t) (sx, sy)) = DrawRequest quad texCoords originMat FanVertexLayout one (TexturingPipeline t)
    where 
        quad = [Vec2 0 0, Vec2 fw 0, Vec2 fw fh, Vec2 0 fh]
        texCoords = Just $ [ Vec2 txStart tyStart
                           , Vec2 txEnd tyStart
                           , Vec2 txEnd tyEnd
                           , Vec2 txStart tyEnd
                           ]

        coordX = num `mod` sx
        coordY = num `div` sx

        txStart = fromIntegral coordX / fromIntegral sx
        tyStart = fromIntegral coordY / fromIntegral sy
        txEnd = fromIntegral (coordX + 1) / fromIntegral sx
        tyEnd = fromIntegral (coordY + 1) / fromIntegral sy

        fw = fromIntegral w / fromIntegral sx
        fh = fromIntegral h / fromIntegral sy
        originMat = calcSpriteOriginMatrix originRef fw fh

-- | Creates a 'DrawRequest' with a rectangle cut out of a sprite
spritePart :: (Vec2, Vec2) -> Sprite -> DrawRequest
spritePart (Vec2 x1 y1, Vec2 x2 y2) (Sprite (w,h) t) = dr
    where
        dr = DrawRequest quad texCoords one FanVertexLayout one (TexturingPipeline t)
        quad = [Vec2 0 0, Vec2 fw 0, Vec2 fw fh, Vec2 0 fh]

        texCoords = Just $ [ Vec2 x1 y1
                           , Vec2 x2 y1
                           , Vec2 x2 y2
                           , Vec2 x1 y2
                           ]

        fw = fromIntegral w * (x2-x1)
        fh = fromIntegral h * (y2-y1)

calcSpriteOriginMatrix :: OriginReference -> Float -> Float -> Mat4
calcSpriteOriginMatrix originRef fw fh =
    case originRef of
        TopLeft -> one
        Middle -> positionToMatrix4 $ Vec2 (-fw/2) (-fh/2)
