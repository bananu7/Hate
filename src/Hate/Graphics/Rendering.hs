{-# LANGUAGE RankNTypes #-}

module Hate.Graphics.Rendering where

import Hate.Common.Types
import Hate.Math
import Hate.Graphics.Util
import Hate.Graphics.Internal
import Hate.Graphics.Pipeline.Util
import Hate.Graphics.Types

import Control.Monad.State

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Vect.Float.OpenGL (orthoMatrix, makeGLMatrix)

-- render takes everything that is needed to output 
render :: DrawRequest -> Action ()
render d = do
    loadVertexListIntoGlobalVertexStream $ vertices d

    
    pip <- case pipeline d of
                SolidColorPipeline _ -> gets solidColorPipeline
                TexturingPipeline -> gets texturingPipeline

    liftIO $ activatePipeline pip

    let orthoScreenMat = orthoMatrix (0, 1024) (0, 768) (-10, 10)
    let requestMat = toMatrix4 . transformation $ d
    let drawMat = (transpose orthoScreenMat) .*. requestMat
    liftIO $ setUniformM4 pip "screen_transformation" drawMat

    let primitiveMode = vertexLayoutToGLLayout $ vertexLayout d
    renderGlobalVertexStream primitiveMode    

vertexLayoutToGLLayout :: VertexLayout -> GL.PrimitiveMode
vertexLayoutToGLLayout FanVertexLayout = GL.TriangleFan
vertexLayoutToGLLayout StripVertexLayout = GL.TriangleStrip

loadVertexListIntoGlobalVertexStream :: [Vec2] -> Action ()
loadVertexListIntoGlobalVertexStream verts = do
    vs <- gets globalVertexStream
    fromVertArrayIntoGlobal verts
    --fromVertArrayInto m rawTexCoords

renderGlobalVertexStream :: GL.PrimitiveMode -> Action ()
renderGlobalVertexStream primitiveMode = do
    vs <- gets globalVertexStream
    liftIO $ do
        GL.bindVertexArrayObject $= Just (vao vs)
        GL.drawArrays primitiveMode 0 (fromIntegral $ vertNum vs)

{-
unpackVerts :: [Vec2] -> [Float]
unpackVerts verts = map realToFrac . concat . map unpackVec $ verts
    where
        unpackVec (Vec2 x y) = [x, y]

-}