import qualified Graphics.Rendering.OpenGL as GL
import Hate
import Hate.Graphics
import Hate.Graphics.Shapes

type SampleState = Int

sampleLoad :: LoadFn SampleState
sampleLoad = return 0

sampleDraw :: DrawFn SampleState
sampleDraw p = [translate (Vec2 150 150) $ circle (fromIntegral p)]

sampleUpdate :: UpdateFn SampleState
sampleUpdate = do
    x <- get
    if x > 50 then put 0
              else put $ x + 1

config :: Config
config = 
    Config
        { windowTitle = "Sample - Shapes"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
