import qualified Graphics.Rendering.OpenGL as GL
import Glisha.G2D
import Glisha

type SampleState = ()
sampleLoad :: LoadFn SampleState
sampleLoad = return ()

sampleDraw :: DrawFn SampleState
sampleDraw = do
    draw $ Polygon [vec2 0 0, vec2 1 0, vec2 0 1]

config :: Config
config = 
    Config
        { windowTitle = "Sample 3"
        , windowSize  = (480, 200)
        }

main :: IO ()
main = runApp config sampleLoad sampleDraw
