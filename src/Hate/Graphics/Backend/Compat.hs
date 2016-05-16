{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Hate.Graphics.Backend.Compat (BackendCompat(), initialGraphicsState) where

import Hate.Graphics.Backend.Compat.Types
import Hate.Graphics.Backend.Compat.Shaders

import Hate.Math
import Hate.Graphics.Rendering
import Hate.Graphics.Backend.Common.Pipeline.Util
import Hate.Graphics.Backend.Common.Pipeline
import Hate.Graphics.Types
import Hate.Graphics.Backend.Common.Util


import Control.Monad.State

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Data.List (groupBy)
import Data.Maybe
import Control.Applicative

instance Renderer BackendCompat where
    --initialRendererState = initialGraphicsState
    render = renderBatch
    contextRequirements _ = DesktopContext 4 4
    updateScreenSize = updateScreenSz

type Action a = (MonadState BackendCompat m, MonadIO m) => m a

initialGraphicsState :: (Int, Int) -> IO BackendCompat
initialGraphicsState screenSz =
    BackendCompat <$> createPipelineNoUniformBindings solidColorPipelineDescs
                  <*> createPipelineNoUniformBindings texturingPipelineDescs
                  <*> createVertexStream
                  <*> pure screenSz

renderBatch :: [DrawRequest] -> Action ()
renderBatch ds = mapM_ (\xs -> renderPipelineBatch (pipeline . head $ xs) xs) $ groupBy equalPipeline ds
    where
        a `equalPipeline` b = pipeline a == pipeline b

renderPipelineBatch :: PipelineDescription -> [DrawRequest] -> Action ()
renderPipelineBatch p ds = do
    pip <- case p of
        SolidColorPipeline color -> do
            pip <- gets solidColorPipeline -- todo set up a proper solid color,
            liftIO $ do
                activatePipeline pip
                colorUniformLocation <- GL.get $ GL.uniformLocation (program pip) "in_color"
                GL.uniform colorUniformLocation $= toOpenGLVertex color
                return pip
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

setScreenTransformationUniform :: Mat4 -> Pipeline -> Action ()
setScreenTransformationUniform t pip = do
    (screenSizeX, screenSizeY) <- gets screenSize

    let orthoScreenMat = orthoMatrix (0, (fromIntegral screenSizeX)) ((fromIntegral screenSizeY), 0) (-10, 10)
    let drawMat = (transpose orthoScreenMat) .*. t
    liftIO $ setUniformM4 pip "screen_transformation" drawMat

renderGlobalVertexStream :: GL.PrimitiveMode -> Action ()
renderGlobalVertexStream primitiveMode = do
    vs <- gets globalVertexStream
    liftIO $ do
        GL.bindVertexArrayObject $= Just (vao vs)
        GL.drawArrays primitiveMode 0 (fromIntegral $ vertNum vs)

updateScreenSz :: (Int, Int) -> Action ()
updateScreenSz sz = modify $ \g -> g { screenSize = sz }

fromVertArrayInto :: ([Vec2], Maybe [Vec2]) -> VertexStream -> Action VertexStream
fromVertArrayInto (verts, maybeTexCoords) s = liftIO $ do
    GL.bindBuffer GL.ArrayBuffer $= Just (vbo s)
    replaceBuffer GL.ArrayBuffer verts

    -- fill in texture coordinates if needed
    let texCoords' = fromMaybe (calculateTexCoords verts) maybeTexCoords
    GL.bindBuffer GL.ArrayBuffer $= Just (texVbo s)
    replaceBuffer GL.ArrayBuffer texCoords'

    return $ s { vertNum = length verts }

fromVertArrayIntoGlobal :: ([Vec2], Maybe [Vec2]) -> Action ()
fromVertArrayIntoGlobal xs = do
    m <- gets globalVertexStream
    m' <- fromVertArrayInto xs m
    modify $ \x -> x { globalVertexStream = m' }
