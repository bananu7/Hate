{-# LANGUAGE TemplateHaskell #-}

import Hate
import Hate.Graphics.Shapes
import Hate.Common.Scheduler

import Control.Lens

data SampleState = SampleState {
    _radius :: Float,
    _sched :: Scheduler SampleState,
    _firstRun :: Bool
}
makeLenses ''SampleState

sampleLoad :: LoadFn SampleState
sampleLoad = return $ SampleState 0 emptyScheduler True

sampleDraw :: DrawFn SampleState
sampleDraw p = [translate (Vec2 150 150) $ circle (p ^. radius)]

schedule' evt = sched %= (flip schedule) evt

every' t evt = every t evt >>= schedule'
after' t evt = after t evt >>= schedule'

sampleUpdate :: UpdateFn SampleState
sampleUpdate = do
    use firstRun >>= \p -> when p $ do
        every' 1 $ radius += 5
        firstRun .= False
    
    use sched >>= process >>= assign sched

    whenKeyPressed Key'Space $ after' 2 $ radius += 20
--    put $ x { radius = radius x + 0.1 }
    --if x > 50 then put 0
    --          else put $ x + 1

config :: Config
config = 
    Config
        { windowTitle = "Sample - Scheduler"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
