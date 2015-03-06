module Hate.Graphics.Backend.Compat where

import Hate.Graphics.Rendering

newtype BackendCompat = BackendCompat Int 

instance Renderer BackendCompat where
    initialRendererState _ = return $ BackendCompat 0
    render _ = return ()
    updateScreenSize _ = return ()
    contextRequirements _ = WebContext