module Hate.Graphics.BackendModern.Types where

import Hate.Graphics.Types

-- supposedly needs more vertex streams and pipelines in the future
data BackendModern = BackendModern { 
    solidColorPipeline :: Pipeline,
    texturingPipeline :: Pipeline,
    globalVertexStream :: VertexStream,
    screenSize :: (Int, Int)
}