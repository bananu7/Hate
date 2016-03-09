{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hate.Graphics.Rendering where

import Hate.Graphics.Types (DrawRequest)
import Control.Monad.State
import Control.Monad.IO.Class

-- | Context type specifies the expectation on surroundings of the backend
-- Desktop means OpenGL, ES - OpenGL ES, and Web - WebGL.
-- Desktop context can require a minor and major version.
data ContextRequirements = DesktopContext Int Int | ESContext Int | WebContext

type ScreenSize = (Int, Int)

-- | A class for renderer backends, i.e. something that can render stuff
class Renderer a where
    contextRequirements  :: a -> ContextRequirements
    --initialRendererState :: ScreenSize -> IO a
    updateScreenSize     :: MonadIO m => a -> ScreenSize -> m ()
    render               :: MonadIO m => a -> [DrawRequest] -> m ()

-- PIMPL MY RIDE
data RendererI = forall a. (Renderer a) => RendererImpl a
