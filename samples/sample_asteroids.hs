{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

import Hate
import Hate.Graphics
import Vec2Lens (x,y)

import Control.Applicative
import Control.Lens
import System.Random

-- sample 4

data EntityState = EntityState {
    _pos :: Vec2,
    _vel :: Vec2,
    _rot :: Float,
    _rotVel :: Float
}
makeLenses ''EntityState

data Asteroid = Asteroid {
    _asteroidEntity :: EntityState,
    _size :: Int
}
makeLenses ''Asteroid
makeFields ''Asteroid

data Player = Player {
    _playerEntity :: EntityState
}
makeLenses ''Player
makeFields ''Player

data SampleState = SampleState {
    _playerSprite :: Sprite,
    _smallAsteroidSprite :: Sprite,
    _mediumAsteroidSprite :: Sprite,
    _bigAsteroidSprite :: Sprite,
    _asteroids :: [Asteroid],
    _player :: Player
}
makeLenses ''SampleState

data VisualRepresentation = SmallAsteroidSprite | MediumAsteroidSprite | BigAsteroidSprite | PlayerSprite
class ToVisual a where
    toVisual :: a -> VisualRepresentation

instance ToVisual Player where
    toVisual (Player _) = PlayerSprite

instance ToVisual Asteroid where
    toVisual (Asteroid _ sz) | sz < 2 = SmallAsteroidSprite
                             | sz < 4 = MediumAsteroidSprite
                             | otherwise = BigAsteroidSprite

randomAsteroids :: Int -> IO [Asteroid] 
randomAsteroids n = replicateM n randomAsteroid

randomAsteroid :: IO Asteroid
randomAsteroid = do
    px <- getStdRandom $ randomR (0,800)
    py <- getStdRandom $ randomR (0,800)
    vx <- getStdRandom $ randomR (-2, 2)
    vy <- getStdRandom $ randomR (-2, 2)
    vr <- getStdRandom $ randomR (-0.1, 0.1)
    sz <- getStdRandom $ randomR (1, 5)

    let e = EntityState (Vec2 px py) (Vec2 vx vy) 0 vr
    return $ Asteroid e sz

initialPlayer = Player $ EntityState (Vec2 200 200) (Vec2 0 0) 0 0

sampleLoad :: LoadFn SampleState
sampleLoad = SampleState <$> loadSprite "samples/asteroids/ship.png" 
                         <*> loadSprite "samples/asteroids/asteroid_small.png" 
                         <*> loadSprite "samples/asteroids/asteroid_medium.png" 
                         <*> loadSprite "samples/asteroids/asteroid_big.png" 
                         <*> randomAsteroids 10
                         <*> pure initialPlayer

sampleDraw :: DrawFn SampleState
sampleDraw s = (map draw (s ^. asteroids)) ++ [draw (s ^. player)]
    where
        draw :: (HasEntity a EntityState, ToVisual a) => a -> DrawRequest
        draw a = (entityToTransform a) . sprite . visualToSprite . toVisual $ a

        visualToSprite SmallAsteroidSprite = s ^. smallAsteroidSprite
        visualToSprite MediumAsteroidSprite = s ^. mediumAsteroidSprite
        visualToSprite BigAsteroidSprite = s ^. bigAsteroidSprite
        visualToSprite PlayerSprite = s ^. playerSprite

        entityToTransform :: HasEntity a EntityState => a -> DrawRequest -> DrawRequest
        entityToTransform a = let e = a ^. entity in (translate (e ^. pos)) . (rotated (e ^. rot))

sampleUpdate :: UpdateFn SampleState
sampleUpdate = do
    asteroids . traversed %= updateEntity
    player %= updateEntity
    steerShip
    where
        updateEntity :: HasEntity a EntityState => a -> a
        updateEntity a = a & (entity . pos +~ a ^. entity . vel)
                           & (entity . rot +~ a ^. entity . rotVel)
                           -- damping:
                           & (entity . vel *~ 0.99)
                           & (entity . rotVel *~ 0.99)
                           & screenWarp

        screenWarp a = a & (if a ^. entity . pos . x > 1024 then entity . pos . x -~ 1024 else id)
                         & (if a ^. entity . pos . y > 768  then entity . pos . y -~ 768 else id)
                         & (if a ^. entity . pos . x < 0 then entity . pos . x +~ 1024 else id)
                         & (if a ^. entity . pos . y < 0 then entity . pos . y +~ 768 else id)


        steerShip = do
            whenKeyPressed Key'Space $ player . entity . vel . y += 1
            whenKeyPressed Key'Left $ player . entity . rotVel += 0.01
            whenKeyPressed Key'Right $ player . entity . rotVel -= 0.01

config :: Config
config = 
    Config
        { windowTitle = "Sample - Asteroids"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
