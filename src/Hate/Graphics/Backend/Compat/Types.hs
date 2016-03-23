module Hate.Graphics.Backend.Compat.Types where

import Hate.Graphics.Types
import Hate.Graphics.Backend.Common.Pipeline

-- supposedly needs more vertex streams and pipelines in the future
data BackendCompat = BackendCompat { 
    solidColorPipeline :: Pipeline,
    texturingPipeline :: Pipeline,
    globalVertexStream :: VertexStream,
    screenSize :: (Int, Int)
}
