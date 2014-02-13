{-# LANGUAGE TemplateHaskell #-}
 
-- game imports 
import Data.List
import Control.Lens
import Control.Monad(unless, when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Applicative((<$>), (<*>))
import Control.Concurrent (threadDelay)
 
-- GLFW imports
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
 
-- file imports
import Util 
  
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description
 
keyCallback :: G.KeyCallback
keyCallback window key scancode action mods =
    when (key == G.Key'Escape && action == G.KeyState'Pressed) $
        G.setWindowShouldClose window True
 
-- actual stuff
data Pipeline = Pipeline {
    _vertexShader   :: Shader,
    _fragmentShader :: Shader,
    _program        :: Program
    }
makeLenses ''Pipeline
 
data Mesh =   Mesh { _vao :: VertexArrayObject,  _vbo :: BufferObject }
            | IndexedMesh { _vao :: VertexArrayObject, _vbo :: BufferObject, _ibo :: BufferObject }
makeLenses ''Mesh
 
-- Mesh holds a lightweight vbo reference, so it is ok to store it "by value"
data Instance = Instance { 
    _mesh :: Mesh, 
    _pipeline :: Pipeline,
    _position :: Vertex2 GLfloat
    }
makeLenses ''Instance
 
fromVertArray :: [GLfloat] -> IO Mesh
fromVertArray verts = 
    Mesh <$> (genObjectName :: IO VertexArrayObject)
         <*> makeBuffer ArrayBuffer verts
 
class Drawable d where
    draw :: d -> IO ()
 
instance Drawable Mesh where
    draw (Mesh vao buffer) = do
        bindVertexArrayObject $= Just vao
        bindBuffer ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
        vertexAttribArray (AttribLocation 0) $= Enabled
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float 0 offset0)
 
        drawArrays TriangleStrip 0 3
 
    draw (IndexedMesh vao vbo ibo) = do
        bindVertexArrayObject $= Just vao
        bindBuffer ArrayBuffer $= Just vbo
        bindBuffer ElementArrayBuffer $= Just ibo
        error "todo"
 
instance Drawable Instance where 
    draw (Instance mesh pip pos) = do 
        let prog = view program pip
        posLoc <- GL.get (uniformLocation prog "instance_position")
 
        currentProgram $= Just prog
        uniform posLoc $= pos
        draw mesh
 
createPipeline :: FilePath -> FilePath -> IO Pipeline
createPipeline vertShaderPath fragShaderPath = do
    vs <- loadShader VertexShader vertShaderPath
    fs <- loadShader FragmentShader fragShaderPath
    prog <- linkShaderProgram [vs, fs]
    return $ Pipeline vs fs prog
 
main :: IO ()
main = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      G.windowHint (G.WindowHint'ContextVersionMajor 3)
      G.windowHint (G.WindowHint'ContextVersionMinor 3)
      G.windowHint (G.WindowHint'OpenGLForwardCompat True)
      G.windowHint (G.WindowHint'OpenGLProfile G.OpenGLProfile'Core)
      G.windowHint (G.WindowHint'OpenGLDebugContext True)
 
      mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          G.setKeyCallback window (Just keyCallback)
 
          pipeline <- createPipeline "shader.vert" "shader.frag"
          let vertexData = [
                -0.6, -0.4,
                 0.6, -0.4,
                 0,    0.6
                 ]
          mesh <- fromVertArray vertexData
          let inst = Instance mesh pipeline (Vertex2 0.5 0.5 :: Vertex2 GLfloat)
 
          mainLoop window inst
 
          G.destroyWindow window
          G.terminate
          exitSuccess          
 
mainLoop :: G.Window -> Instance -> IO ()
mainLoop w i = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]
    
    Just t <- G.getTime
    --rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)
   
    draw i
        
    G.swapBuffers w
    G.pollEvents
    mainLoop w i
 
{-
data Point = Point { _x :: Int, _y :: Int } deriving (Eq, Show)
data Player = Player { _position :: Point } deriving (Eq, Show)
makeLenses ''Player
makeLenses ''Point
 
data Field = FieldBlocked | FieldEmpty deriving (Eq, Show) 
type Board = [[Field]]
data Game = Game { _player :: Player, _board :: Board }
makeLenses ''Game
 
boardSize = 5
emptyBoard = (replicate boardSize) . (replicate boardSize) $ FieldEmpty
newGame = Game { 
    _board = emptyBoard,
    _player = Player (Point half half)
    }
    where half = boardSize `div` 2
 
update = do
    zoom(player.position) $ do
        x += 1
        y -= 1
 
draw game = print $ _player game
 
main = do
    runStateT loop newGame where 
       loop = forever $ do
           update
           get >>= liftIO . draw
           liftIO $ threadDelay $ 1000*1000
 
-}
