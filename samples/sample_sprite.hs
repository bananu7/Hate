{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Hate
import Hate.Graphics
import Vec2Lens (x,y)

import Control.Applicative
import Control.Lens
import System.Random

-- sample 4

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
sampleDraw s = map (\(Sehe p v) -> translate p $ sprite TopLeft (s ^. seheSprite)) $ s ^. sehes

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
sampleUpdate _ = sehes . traverse %= moveSehe

config :: Config
config = 
    Config
        { windowTitle = "Sample - Sprite"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
