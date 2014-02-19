import qualified Graphics.Rendering.OpenGL as GL
import Glisha2D
import qualified Graphics.UI.GLFW as GLFW

import Control.Lens
import Control.Monad
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
    --go <- liftM $ glishaGetKey GLFW.Key'Space

    --if go then do

    objects <- glishaGetUserState
    mapM_ glishaDraw objects

    let change x = if x < 1.0 then x + 0.01
                              else x - 2.0
        getX = position.(element 0)
        everyX = traversed . getX

    let objects' = everyX `over` change $ objects

    glishaPutUserState objects'

    --traversed.position.(element 0) %= \x -> if x < 1.0 then x+0.01   
    --                                                   else x-2.0
                                                       
    return ()

main = runApp sampleLoad sampleDraw

