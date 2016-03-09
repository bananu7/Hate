module Hate.Graphics.Backend.Compat.Types where

import Hate.Graphics.Types
import Hate.Graphics.Pipeline
import Data.IORef

data BackendCompat = BackendCompat (IORef BackendCompatState)

-- supposedly needs more vertex streams and pipelines in the future
data BackendCompatState = BackendCompatState { 
    solidColorPipeline :: Pipeline,
    texturingPipeline :: Pipeline,
    globalVertexStream :: VertexStream,
    screenSize :: (Int, Int)
}
