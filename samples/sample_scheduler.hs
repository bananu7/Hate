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
sampleUpdate _ = do
    -- repeated action
    use firstRun >>= \p -> when p $ do
        every' 1 $ radius += 50
        firstRun .= False
        
    use sched >>= process >>= assign sched

    -- queue a waiting action
    whenKeyPressed Key'Space $ after' 2 $ radius += 20

    -- continuous action
    radius *= 0.95

config :: Config
config =
    Config
        { windowTitle = "Sample - Scheduler"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
