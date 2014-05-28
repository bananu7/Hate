
import qualified Graphics.Rendering.OpenGL as GL
import Glisha.G2D
import Glisha

type SampleState = Sprite
sampleLoad :: LoadFn SampleState
sampleLoad = fmap sprite $ loadTexture "image.png"

sampleDraw :: DrawFn SampleState
sampleDraw = do
    --get >>= draw
    draw $ Polygon [vec2 0 0, vec2 1 0, vec2 0 1]

main = runApp sampleLoad sampleDraw

