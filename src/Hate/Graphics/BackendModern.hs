{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Backend45 where

import Hate.Graphics.BackendModern.Types

import Hate.Math
import Hate.Graphics.Rendering
import Hate.Graphics.Internal
import Hate.Graphics.Pipeline.Util
import Hate.Graphics.Pipeline
import Hate.Graphics.Types

import Control.Monad.State

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Vect.Float.OpenGL (orthoMatrix)
import Data.List (groupBy)

type instance Renderer BackendModern where
    type RendererState = BackendModern
    render = renderBatch

renderBatch :: [DrawRequest] -> Action ()
renderBatch ds = mapM_ (\xs -> renderPipelineBatch (pipeline . head $ xs) xs) $ groupBy equalPipeline ds
    where
        a `equalPipeline` b = pipeline a == pipeline b

renderPipelineBatch :: PipelineDescription -> [DrawRequest] -> Action ()
renderPipelineBatch p ds = do
    pip <- case p of
        SolidColorPipeline _ -> gets solidColorPipeline -- todo set up a proper solid color
        TexturingPipeline texId -> do
            pip <- gets texturingPipeline 
            liftIO $ activatePipeline pip
            liftIO $ GL.textureBinding GL.Texture2D $= Just texId
            return pip

    forM_ ds $ \d -> do
        let mat = transformation d .*. origin d
        setScreenTransformationUniform mat pip

        fromVertArrayIntoGlobal (vertices d, texCoords d)
        let primitiveMode = vertexLayoutToGLLayout $ vertexLayout d
        renderGlobalVertexStream primitiveMode

-- render takes everything that is needed to output 
singularRender :: DrawRequest -> Action ()
singularRender d = do
    pip <- case pipeline d of
                SolidColorPipeline _ -> gets solidColorPipeline -- todo set up a proper solid color
                TexturingPipeline _ -> gets texturingPipeline -- todo use tex information to pick appropriate texture

    liftIO $ activatePipeline pip
    
    let mat = transformation d .*. origin d
    setScreenTransformationUniform mat pip

    fromVertArrayIntoGlobal (vertices d, texCoords d)
    let primitiveMode = vertexLayoutToGLLayout $ vertexLayout d
    renderGlobalVertexStream primitiveMode

setScreenTransformationUniform :: Mat4 -> Pipeline -> Action ()
setScreenTransformationUniform t pip = do
    (screenSizeX, screenSizeY) <- gets screenSize

    let orthoScreenMat = orthoMatrix (0, (fromIntegral screenSizeX)) ((fromIntegral screenSizeY), 0) (-10, 10)
    let drawMat = (transpose orthoScreenMat) .*. t
    liftIO $ setUniformM4 pip "screen_transformation" drawMat

vertexLayoutToGLLayout :: VertexLayout -> GL.PrimitiveMode
vertexLayoutToGLLayout FanVertexLayout = GL.TriangleFan
vertexLayoutToGLLayout StripVertexLayout = GL.TriangleStrip
vertexLayoutToGLLayout LinesVertexLayout = GL.Lines

renderGlobalVertexStream :: GL.PrimitiveMode -> Action ()
renderGlobalVertexStream primitiveMode = do
    vs <- gets globalVertexStream
    liftIO $ do
        GL.bindVertexArrayObject $= Just (vao vs)
        GL.drawArrays primitiveMode 0 (fromIntegral $ vertNum vs)
