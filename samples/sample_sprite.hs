{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Graphics.Rendering.OpenGL as GL
import Hate
import Hate.Graphics

import Control.Applicative
import Control.Lens hiding (_1, _2)
import System.Random

-- sample 4

x = lens (_1) setX
y = lens (_2) setY

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
generateSehes = replicateM 10000 generateSehe

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
        updateVel s = s & vel .~ newVel
            where
                newVel = Vec2 nx ny
                nx = if px < 0 || px > (1024 - 128) then negate oldX else oldX
                ny = if py < 0 || py > (768 - 128) then negate oldY else oldY
                oldX = s ^. vel . x
                oldY = s ^. vel . y
                px = s ^. pos . x
                py = s ^. pos . y

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
