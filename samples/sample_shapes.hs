import Hate
import Hate.Graphics.Api.Shapes

data SampleState = SampleState {
    r :: Int,
    p :: Vec2
}

sampleLoad :: LoadFn SampleState
sampleLoad = return $ SampleState 0 (Vec2 150 150)

green :: Float -> Vec4
green e = Vec4 0.0 e 0.0 1.0

sampleDraw :: DrawFn SampleState
sampleDraw (SampleState r p) =
    [ translate p $ colored (green $ (fromIntegral r) / 150.0) $ circle (fromIntegral r)
    , line p (Vec2 300 300)
    ]

sampleUpdate :: UpdateFn SampleState
sampleUpdate events = do
    modify $ \s -> if r s > 150 then s { r = 0 }
                                else s { r = r s + 1 }

    forM_ events $ \e -> case e of
        EventCursorPos x y -> modify $ \s -> s { p = Vec2 x y }
        _ -> return ()

config :: Config
config =
    Config
        { windowTitle = "Sample - Shapes"
        , windowSize  = (1280, 768)
        }

main :: IO ()
main = runApp config sampleLoad sampleUpdate sampleDraw
