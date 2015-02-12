import qualified Graphics.Rendering.OpenGL as GL
import Hate
import Hate.Graphics
import Hate.Graphics.Shapes

type SampleState = Int

sampleLoad :: LoadFn SampleState
sampleLoad = return 0

sampleDraw :: DrawFn SampleState
sampleDraw = withGlobalPipeline $ do
    p <- fmap fromIntegral ask
    --draw $ PolygonWireframe $ Polygon [vec2 10 10, vec2 10 20, vec2 20 20]
    --line (vec2 0 0) (vec2 1 1)
    
    draw $ circle Filled (vec2 50 50) p

sampleUpdate :: UpdateFn SampleState
sampleUpdate = do
    x <- get
    if x > 50 then put 0
              else put $ x + 1

config :: Config
config = 
    Config
        { windowTitle = "Sample 3"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
