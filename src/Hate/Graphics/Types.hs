 module Hate.Graphics.Types where

import Hate.Graphics.Pipeline
import Hate.Math.Types

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vect.Float
import Data.Vect.Float.Instances()

data GraphicsState = GraphicsState { 
        mainPipeline :: Pipeline,
        globalMesh :: Mesh
    }

-- |A general type for a graphical mesh, either in indexed or raw form.
data Mesh =   Mesh { vao :: GL.VertexArrayObject,  vbo :: GL.BufferObject, vertNum :: Int }
            | IndexedMesh { vao :: GL.VertexArrayObject, vbo :: GL.BufferObject, ibo :: GL.BufferObject, vertNum :: Int }

newtype MeshWireframe = MeshWireframe Mesh

-- Mesh holds a lightweight vbo reference, so it is ok to store it "by value"
{- |Instance object is a Mesh bundled with a pipeline that is to be used to render it, and
 - its position in the world coordinates -}
data Instance = Instance { 
    mesh :: Mesh, 
    pipeline :: Pipeline,
    position :: GL.Vertex2 GL.GLfloat
    }

data Polygon = Polygon [Vec2]
newtype PolygonWireframe = PolygonWireframe Polygon

data Sprite = Sprite {
    transformation :: Transformation,
    size :: Vec2,
    texture :: GL.TextureObject
    }
