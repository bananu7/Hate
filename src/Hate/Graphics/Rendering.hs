{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Hate.Graphics.Rendering where

import Hate.Common.Types (HateDraw)
import Hate.Graphics.Types (DrawRequest)

-- | Context type specifies the expectation on surroundings of the backend
-- Desktop means OpenGL, ES - OpenGL ES, and Web - WebGL.
-- Desktop context can require a minor and major version.
data ContextRequirements = DesktopContext Int Int | ESContext Int | WebContext

-- | A class for renderer backends, i.e. something that can render stuff
class Renderer a where
    type RendererState
    contextRequirements :: a -> ContextRequirements
    render :: [DrawRequest] -> forall us. HateDraw us ()
