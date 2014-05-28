import qualified Graphics.Rendering.OpenGL as GL
import Glisha.G3D
import Glisha

type SampleState = Instance
sampleLoad :: LoadFn SampleState
sampleLoad = do
    pipeline <- createPipeline "shader.vert" "shader.frag"
    let vertexData = [
            -0.6, -0.4,
            0.6, -0.4,
            0,    0.6
            ]
    mesh <- fromVertArray vertexData
    let inst = Instance mesh pipeline (GL.Vertex2 0.5 0.5 :: GL.Vertex2 GL.GLfloat)
    return inst

sampleDraw :: DrawFn SampleState
sampleDraw = get >>= draw

main = runApp sampleLoad sampleDraw

