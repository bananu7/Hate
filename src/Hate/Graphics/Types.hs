 module Hate.Graphics.Types where

import Hate.Graphics.Pipeline

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vect.Float
import Data.Vect.Float.Instances()

-- |A general type for a graphical mesh, either in indexed or raw form.
data VertexStream = VertexStream {
    vao :: GL.VertexArrayObject,
    vbo :: GL.BufferObject,
    texVbo :: GL.BufferObject,
    vertNum :: Int
} deriving (Eq, Show)

-- userspace data
data PipelineDescription = SolidColorPipeline Vec4 | TexturingPipeline GL.TextureObject deriving (Eq, Show)

data VertexLayout = FanVertexLayout | StripVertexLayout | LinesVertexLayout deriving (Eq, Show)

data OriginReference = TopLeft | Middle deriving (Eq, Show)

data DrawRequest = DrawRequest { 
    vertices :: [Vec2],
    texCoords :: Maybe [Vec2],
    origin :: Mat4,
    vertexLayout :: VertexLayout,
    transformation :: Mat4,
    pipeline :: PipelineDescription
} deriving (Eq, Show)

data Sprite = Sprite {
    size :: (Int, Int),
    texture :: GL.TextureObject
} deriving (Eq, Show)

data SpriteSheet = SpriteSheet {
    spriteRef :: Sprite,
    sheetSize :: (Int, Int)
}
