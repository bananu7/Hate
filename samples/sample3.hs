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
    p <- fmap fromIntegral ask
    draw $ PolygonWireframe $ Polygon [vec2 10 10, vec2 10 20, vec2 20 20]
    --line (vec2 0 0) (vec2 1 1)
    
    --circle Filled (vec2 (p/10.0 + 10) 50) 2

sampleUpdate :: UpdateFn SampleState
sampleUpdate = return () --modify (+1)

config :: Config
config = 
    Config
        { windowTitle = "Sample 3"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
