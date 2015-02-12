 module Hate.Graphics.Types where

import Hate.Graphics.Pipeline
import Hate.Math.Types

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vect.Float
import Data.Vect.Float.Instances()

-- supposedly needs more vertex streams and pipelines in the future
data GraphicsState = GraphicsState { 
        solidColorPipeline :: Pipeline,
        texturingPipeline :: Pipeline,
        globalVertexStream :: VertexStream
    }

-- |A general type for a graphical mesh, either in indexed or raw form.
data VertexStream = VertexStream { vao :: GL.VertexArrayObject,  vbo :: GL.BufferObject, vertNum :: Int }

-- userspace data
data PipelineDescription = SolidColorPipeline Vec4 | TexturingPipeline

data VertexLayout = FanVertexLayout | StripVertexLayout

data DrawRequest = DrawRequest { 
    vertices :: [Vec2],
    vertexLayout :: VertexLayout,
    normals :: Maybe [Vec2],
    transformation :: Transformation,
    pipeline :: PipelineDescription
}

data Sprite = Sprite {
    size :: (Int, Int),
    texture :: GL.TextureObject
    }
