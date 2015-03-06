module Hate.Graphics.Backend.Compat
    (BackendCompat()
    )
where

import Hate.Graphics.Rendering
import Hate.Graphics.Backend.Util
import Hate.Graphics.Pipeline.Util
import Hate.Graphics.Pipeline

import Control.Applicative

data BackendCompat = BackendCompat { 
    solidColorPipeline :: Pipeline,
    texturingPipeline :: Pipeline,
    globalVertexStream :: VertexStream,
    screenSize :: (Int, Int)
}

instance Renderer BackendCompat where
    initialRendererState _ = initialGraphicsState
    render = mapM_ singularRender
    updateScreenSize _ = updateScreenSz
    contextRequirements _ = DesktopContext 3 3

initialGraphicsState :: (Int, Int) -> IO BackendCompat
initialGraphicsState screenSz =
    BackendCompat <$> createPipelineFromSources solidColorPipelineSources
                  <*> createPipelineFromSources texturingPipelineSources
                  <*> createVertexStream
                  <*> pure screenSz

-- render takes everything that is needed to output 
singularRender :: DrawRequest -> Action ()
singularRender d = do
    pip <- case pipeline d of
                SolidColorPipeline _ -> gets solidColorPipeline -- todo set up a proper solid color
                TexturingPipeline _ -> gets texturingPipeline

    liftIO $ activatePipeline pip
    
    let mat = transformation d .*. origin d
    setScreenTransformationUniform mat pip

    fromVertArrayIntoGlobal (vertices d, texCoords d)
    let primitiveMode = vertexLayoutToGLLayout $ vertexLayout d
    renderGlobalVertexStream primitiveMode

updateScreenSz :: (Int, Int) -> Action ()
updateScreenSz sz = modify $ \g -> g { screenSize = sz }