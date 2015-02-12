import qualified Graphics.Rendering.OpenGL as GL
import Hate
import Hate.Graphics

import Control.Applicative

-- sample 4

type SampleState = Sprite

sampleLoad :: LoadFn SampleState
sampleLoad = loadSprite "samples/image.png"

sampleDraw :: DrawFn SampleState
sampleDraw s = [sprite s]

sampleUpdate :: UpdateFn SampleState
sampleUpdate = return ()

config :: Config
config = 
    Config
        { windowTitle = "Sample - Sprite"
        , windowSize  = (1024, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
