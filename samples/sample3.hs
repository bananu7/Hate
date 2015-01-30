import qualified Graphics.Rendering.OpenGL as GL
import Hate
import Hate.Graphics
import Hate.Graphics.Shapes

type SampleState = Int

sampleLoad :: LoadFn SampleState
sampleLoad = return 0

sampleDraw :: DrawFn SampleState
sampleDraw = do
    activateGlobalPipeline
    --draw $ PolygonWireframe $ Polygon [vec2 0 0, vec2 1 0, vec2 0 1]
    --line (vec2 0 0) (vec2 1 1)
    p <- fmap fromIntegral ask
    circle Filled (vec2 (p/10.0) 0) 2

sampleUpdate :: UpdateFn SampleState
sampleUpdate = modify (+1)

config :: Config
config = 
    Config
        { windowTitle = "Sample 3"
        , windowSize  = (480, 200)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
