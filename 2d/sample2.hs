import qualified Graphics.Rendering.OpenGL as GL
import Glisha2D

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

type SampleState = [Instance]

sampleLoad :: LoadFn SampleState
sampleLoad = do
    pipeline <- createPipeline "shader.vert" "shader.frag"
    let vertexData = [
            -0.6, -0.4,
            0.6, -0.4,
            0,    0.6
            ]
    mesh <- fromVertArray vertexData

    let inst = [createInst (-0.5, -0.5),
                createInst (-0.5,  0.5),
                createInst (0.5,  -0.5),
                createInst (0.5,   0.5)]
            where createInst (x,y) = Instance mesh pipeline (GL.Vertex2 x y :: GL.Vertex2 GL.GLfloat)
   
    return inst

sampleDraw :: DrawFn SampleState
sampleDraw = do
    get >>= liftIO . (mapM_ draw)
    traversed.position.(element 0) += 0.01   

main = runGlisha sampleLoad sampleDraw

