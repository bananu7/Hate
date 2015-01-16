{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hate.Graphics.Shapes where

import Hate.Graphics
import Hate.Common
import Hate.Math

import Control.Monad.State
import Control.Applicative

data DrawMode = Filled | Outline deriving(Show, Eq)
type Action = forall us. Hate us ()

--line :: Vec2 -> Vec2 -> Action
--line a b = draw $ Polygon [a, b]

newtype Hate2D us a = Hate2D { runHate2D :: Hate us a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState GraphicsState (Hate2D us) where
    get = Hate2D $ UnsafeHate $ graphicsState <$> gets libraryState
    put x = Hate2D $ UnsafeHate $ do
        g <- get
        let ls = libraryState g
        put $ g { libraryState = ls { graphicsState = x } }

circle :: DrawMode -> Vec2 -> Float -> Action
circle _ p r = draw $ Polygon verts 
    where 
          verts = map ((+p) . (flip rotate $ vec2 0 r)) $ angles
          angles = map ((* pi2) . (/ segNum)) $ [0..(segNum-1)] 
          pi2 = 2 * pi
          segNum = 100
 
test :: Action
test = runHate2D $ do
    p <- liftIO solidColorPipeline
    put $ GraphicsState p



