{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Hate
import Hate.Graphics
import Vec2Lens (x,y)

import Control.Applicative
import Control.Lens
import System.Random

-- sample 4

data Koala = Koala {
    _pos :: Vec2,
    _num :: Int
}
makeLenses ''Koala

data SampleState = SampleState {
    _koalaSpriteSheet :: Sprite,
    _koalas :: [Koala]
}
makeLenses ''SampleState

generateKoalas :: IO [Koala] 
generateKoalas = mapM generateKoala [0..24]

generateKoala :: Int -> IO Koala
generateKoala i = do
    let px = 128 * (fromIntegral $ i `div` 5)
    let py = 128 * (fromIntegral $ i `mod` 5)
    n <- getStdRandom $ randomR (0 :: Int,3)

    return $ Koala (Vec2 px py) n

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprite "samples/nooble.png" 
                         <*> generateKoalas

sampleDraw :: DrawFn SampleState
sampleDraw s = map (\(Koala p n) -> translate p $ spriteSheet n (2,2) (s ^. koalaSpriteSheet)) $ s ^. koalas

sampleUpdate :: UpdateFn SampleState
sampleUpdate = koalas . traverse . num %= \n -> if n > 3 then 0 else n+1

config :: Config
config = 
    Config
        { windowTitle = "Sample - Sprite"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
