import qualified Graphics.Rendering.OpenGL as GL
import Glisha2D
import qualified Graphics.UI.GLFW as GLFW

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

type SampleState = [Instance]

hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

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

    objects <- getUserState
    mapM_ glishaDraw objects

    let change x = if x < 1.0 then x + 0.01
                              else x - 2.0
        x = element 0
        getX = position.(element 0)
        everyX = traversed . getX

        changeS :: StateT SampleState (Glisha SampleState) ()
        changeS = everyX %= change
        --everyY = traversed . position . (element 1)

    let objects' = everyX `over` change $ objects
    let objectsS = execStateT changeS objects
    --let objects2 = execStateT (everyX %= change) objects

    --putUserState objects'

    --user $ everyX %= change
    user $ do
        traversed.position.x %= change

    return ()

main = runApp sampleLoad sampleDraw

