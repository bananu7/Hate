module Hate.Graphics.Backend 
    ( RendererI(..)
    , initialRendererStateModern
    , initialRendererStateCompat
    )
where

import qualified Hate.Graphics.Backend.Modern as Modern
import qualified Hate.Graphics.Backend.Compat as Compat

import Hate.Graphics.Rendering

import Control.Monad.State

instance Renderer RendererI where
    contextRequirements (RendererImpl a) = contextRequirements a
    --initialRendererState s = fmap RendererImpl $ initialRendererState s
    updateScreenSize (RendererImpl r) s = updateScreenSize r s
    render (RendererImpl r) x = render r x

initialRendererStateModern :: (Int, Int) -> IO RendererI
initialRendererStateCompat :: (Int, Int) -> IO RendererI

initialRendererStateModern s = fmap RendererImpl $ Modern.initialGraphicsState s
initialRendererStateCompat s = fmap RendererImpl $ Compat.initialGraphicsState s