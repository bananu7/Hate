{-# LANGUAGE TemplateHaskell #-}

module Glisha2D where

-- control & data imports
import Data.List
import Control.Lens
import Control.Monad(unless, when)
import Control.Applicative((<$>), (<*>))
import Control.Concurrent (threadDelay)
 
-- GL & OS imports
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
 
-- file imports
import Util 
  
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
 
 
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description
 
keyCallback :: G.KeyCallback
keyCallback window key scancode action mods =
    when (key == G.Key'Escape && action == G.KeyState'Pressed) $
        G.setWindowShouldClose window True

type LoadFn userStateType = IO userStateType
type DrawFn userStateType = StateT userStateType IO ()

glishaInit :: IO G.Window
glishaInit = do
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

glishaLoop :: G.Window -> DrawFn us -> us -> IO ()
glishaLoop w drawFn us = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]
    
    Just t <- G.getTime

    -- call user drawing function
    us' <- execStateT drawFn us
   
    G.swapBuffers w
    G.pollEvents
    glishaLoop w drawFn us'

runGlisha :: LoadFn us -> DrawFn us -> IO ()
runGlisha loadFn drawFn = do
    window <- glishaInit
    userState <- loadFn

    glishaLoop window drawFn userState

    glishaSuccessfulExit window

