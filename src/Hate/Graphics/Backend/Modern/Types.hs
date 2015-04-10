module Hate.Graphics.Backend.Modern.Types where

import Hate.Graphics.Types
import Hate.Graphics.Pipeline

-- supposedly needs more vertex streams and pipelines in the future
data BackendModern = BackendModern { 
    solidColorPipeline :: Pipeline,
    texturingPipeline :: Pipeline,
    globalVertexStream :: VertexStream,
    screenSize :: (Int, Int)
}