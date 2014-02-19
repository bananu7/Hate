{-# LANGUAGE TemplateHaskell #-}

module Glisha2D where

-- control & data imports
import Data.List
import Control.Lens
import Control.Monad(unless, when)
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Applicative((<$>), (<*>))
import Control.Concurrent (threadDelay)
 
-- GL & OS imports
import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
 
-- file imports
import Util 
  
-- actual stuff
data Pipeline = Pipeline {
    _vertexShader   :: GL.Shader,
    _fragmentShader :: GL.Shader,
    _program        :: GL.Program
    }
makeLenses ''Pipeline
 
data Mesh =   Mesh { _vao :: GL.VertexArrayObject,  _vbo :: GL.BufferObject }
            | IndexedMesh { _vao :: GL.VertexArrayObject, _vbo :: GL.BufferObject, _ibo :: GL.BufferObject }
makeLenses ''Mesh
 
-- Mesh holds a lightweight vbo reference, so it is ok to store it "by value"
data Instance = Instance { 
    _mesh :: Mesh, 
    _pipeline :: Pipeline,
    _position :: GL.Vertex2 GL.GLfloat
    }
makeLenses ''Instance
 
fromVertArray :: [GL.GLfloat] -> IO Mesh
fromVertArray verts = 
    Mesh <$> (GL.genObjectName :: IO GL.VertexArrayObject)
         <*> makeBuffer GL.ArrayBuffer verts
 
class Drawable d where
    draw :: d -> IO ()
 
instance Drawable Mesh where
    draw (Mesh vao buffer) = do
        GL.bindVertexArrayObject $= Just vao
        GL.bindBuffer GL.ArrayBuffer $= (Just buffer) -- (vertexBuffer buffer)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 offset0)
 
        GL.drawArrays GL.TriangleStrip 0 3
 
    draw (IndexedMesh vao vbo ibo) = do
        GL.bindVertexArrayObject $= Just vao
        GL.bindBuffer GL.ArrayBuffer $= Just vbo
        GL.bindBuffer GL.ElementArrayBuffer $= Just ibo
        error "todo"
 
instance Drawable Instance where 
    draw (Instance mesh pip pos) = do 
        let prog = view program pip
        posLoc <- GL.get (GL.uniformLocation prog "instance_position")
 
        GL.currentProgram $= Just prog
        GL.uniform posLoc $= pos
        draw mesh
 
createPipeline :: FilePath -> FilePath -> IO Pipeline
createPipeline vertShaderPath fragShaderPath = do
    vs <- loadShader GL.VertexShader vertShaderPath
    fs <- loadShader GL.FragmentShader fragShaderPath
    prog <- linkShaderProgram [vs, fs]
    return $ Pipeline vs fs prog
 
type LoadFn userStateType = IO userStateType
type DrawFn userStateType = StateT userStateType IO ()
{-
type KeyCallbackFn us = G.Key -> StateT us IO ()
data Callbacks us = Callbacks { onKeyUp :: KeyCallbackFn us, onKeyDown :: KeyCallbackFn }
emptyKeyCallback _ = return ()
defaultCallbacks = Callbacks { onKeyUp = emptyKeyCallback, onKeyDown = emptyKeyCallback }
-}

data GlishaState us = GlishaState { userState :: us, window :: G.Window, drawFn :: DrawFn us }
type Glisha us = StateT (GlishaState us) IO () 

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description
 
keyCallback :: G.KeyCallback
keyCallback window key scancode action mods =
    when (key == G.Key'Escape && action == G.KeyState'Pressed) $
        G.setWindowShouldClose window True        

glishaInitWindow :: IO G.Window
glishaInitWindow = do
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
          return window


glishaSuccessfulExit window = do
    G.destroyWindow window
    G.terminate
    exitSuccess          

glishaLoop :: Glisha us
glishaLoop = do
    gs <- get
    let w = window gs

    shouldClose <- (liftIO . G.windowShouldClose) w
    if not shouldClose then do 
        liftIO $ do
            (width, height) <- G.getFramebufferSize w
            let ratio = fromIntegral width / fromIntegral height
    
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
            GL.clear [GL.ColorBuffer]
    
            --Just t <- G.getTime

            -- call user drawing function
        let us = userState gs
        let dfn = drawFn gs
        us' <- liftIO $ execStateT dfn us
        -- TODO: MORE LENSES
        put $ gs { userState = us' } 
   
        liftIO $ do 
            G.swapBuffers w
            G.pollEvents
        glishaLoop 

      else return ()

glishaGetKey k = do
    gs <- get
    state <- liftIO $ G.getKey (window gs) k 
    return state

runGlisha :: LoadFn us -> DrawFn us -> IO ()
runGlisha loadFn drawFn = do
    window <- glishaInitWindow
    userState <- loadFn

    evalStateT glishaLoop $ GlishaState userState window drawFn

    glishaSuccessfulExit window

