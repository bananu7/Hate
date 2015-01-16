{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Glisha.Graphics.Drawable.Class where

import Glisha.Common.Types

-- |Anything that can be drawn, basically
class Drawable d where
    draw :: forall us. d -> Glisha us ()