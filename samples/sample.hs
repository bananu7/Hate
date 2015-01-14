import qualified Graphics.Rendering.OpenGL as GL
import Glisha.G3D
import Glisha
import Glisha.Pipeline

type SampleState = Instance

sampleLoad :: LoadFn SampleState
sampleLoad = do
    pipeline <- createPipeline "samples/shaders/shader.vert" "samples/shaders/shader.frag"
    let vertexData = [
            -1, -1,
            1, -1,
            1,  1
            ]
    mesh <- fromVertArray vertexData
    let inst = Instance mesh pipeline (GL.Vertex2 0.0 0.0 :: GL.Vertex2 GL.GLfloat)
    return inst

sampleDraw :: DrawFn SampleState
sampleDraw = get >>= draw

config :: Config
config = 
    Config
        { windowTitle = "Sample"
        , windowSize  = (480, 200)
        }

main :: IO ()
main = runApp config sampleLoad sampleDraw
