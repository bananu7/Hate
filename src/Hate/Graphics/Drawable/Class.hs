{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Hate.Graphics.Drawable.Class where

import Hate.Common.Types

-- |Anything that can be drawn, basically
class Drawable d where
    draw :: forall us. d -> HateDraw us ()