import qualified Graphics.Rendering.OpenGL as GL
import Glisha2D
import qualified Graphics.UI.GLFW as GLFW

import Control.Lens
import Control.Monad.State

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
    objects <- get
    mapM_ glishaDraw objects

    let change x = if x < 1.0 then x + 0.01
                              else x - 2.0
        x = element 0

    go <- glishaGetKey GLFW.Key'Space
    when go $
        traversed.position.x %= change

main = runApp sampleLoad sampleDraw

