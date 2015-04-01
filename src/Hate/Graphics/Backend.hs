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
    updateScreenSize s = do
        (RendererImpl a) <- get
        a' <- execStateT (updateScreenSize s) a
        put $ RendererImpl a'

    render x = do
        (RendererImpl a) <- get
        a' <- execStateT (render x) a
        put $ RendererImpl a'


initialRendererStateModern s = fmap RendererImpl $ Modern.initialGraphicsState s
initialRendererStateCompat s = fmap RendererImpl $ Compat.initialGraphicsState s