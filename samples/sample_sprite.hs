{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Graphics.Rendering.OpenGL as GL
import Hate
import Hate.Graphics

import Control.Applicative
import Control.Lens
import System.Random

-- sample 4

x :: Simple Lens Vec2 Float
x = lens getX setX

y :: Simple Lens Vec2 Float
y = lens getY setY

getX (Vec2 x _) = x
getY (Vec2 _ y) = y

setX (Vec2 x y) nx = Vec2 nx y
setY (Vec2 x y) ny = Vec2 x ny

data Sehe = Sehe {
    _pos :: Vec2,
    _vel :: Vec2
}
makeLenses ''Sehe

data SampleState = SampleState {
    _seheSprite :: Sprite,
    _sehes :: [Sehe]
}
makeLenses ''SampleState

generateSehes :: IO [Sehe] 
generateSehes = replicateM 100 generateSehe

generateSehe :: IO Sehe
generateSehe = do
    px <- getStdRandom $ randomR (0,300)
    py <- getStdRandom $ randomR (0,300)
    vx <- getStdRandom $ randomR (-5, 5)
    vy <- getStdRandom $ randomR (-5, 5)

    return $ Sehe (Vec2 px py) (Vec2 vx vy)

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprite "samples/image.png" 
                         <*> generateSehes

sampleDraw :: DrawFn SampleState
sampleDraw s = map (\(Sehe p v) -> translate p $ sprite (s ^. seheSprite)) $ s ^. sehes

moveSehe :: Sehe -> Sehe
moveSehe = updatePos . updateVel
    where
        updateVel :: Sehe -> Sehe
        updateVel s = s & (if bounceX then vel . x %~ negate else id)
                        & (if bounceY then vel . y %~ negate else id)
            where
                bounceX = outOfBounds (s ^. pos . x) (0, 1024 - 128)
                bounceY = outOfBounds (s ^. pos . y) (0, 786 - 128)
                outOfBounds v (lo, hi) = v < lo || v > hi

        updatePos :: Sehe -> Sehe
        updatePos (Sehe p v) = Sehe (p + v) v

sampleUpdate :: UpdateFn SampleState
sampleUpdate = sehes . traverse %= moveSehe

config :: Config
config = 
    Config
        { windowTitle = "Sample - Sprite"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
