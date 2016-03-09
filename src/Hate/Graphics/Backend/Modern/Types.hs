module Hate.Graphics.Backend.Modern.Types where

import Hate.Graphics.Types
import Hate.Graphics.Pipeline
import Data.IORef

data BackendModern = BackendModern (IORef BackendModernState)

-- supposedly needs more vertex streams and pipelines in the future
data BackendModernState = BackendModernState { 
    solidColorPipeline :: Pipeline,
    texturingPipeline :: Pipeline,
    globalVertexStream :: VertexStream,
    screenSize :: (Int, Int)
}