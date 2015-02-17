import Hate
import Hate.Graphics.Shapes
import Hate.Common.Scheduler

data SampleState = SampleState {
    radius :: Float,
    sched :: Scheduler SampleState,
    firstRun :: Bool
}

sampleLoad :: LoadFn SampleState
sampleLoad = return $ SampleState 0 emptyScheduler True

sampleDraw :: DrawFn SampleState
sampleDraw p = [translate (Vec2 150 150) $ circle (radius p)]

schedule' evt = do
    x <- get
    put $ x { sched = schedule (sched x) evt }

sampleUpdate :: UpdateFn SampleState
sampleUpdate = do
    gets firstRun >>= \p -> when p $ do
        (every 1 $ modify (\x -> x { radius = radius x + 5 })) >>= schedule'
        modify (\x -> x { firstRun = False })
    
    sched' <- gets sched
    sched'' <- process sched'

    modify $ \x -> x { sched = sched'' }

    whenKeyPressed Key'Space $ (after 2 $ modify (\x -> x { radius = radius x - 20 })) >>= schedule'
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
