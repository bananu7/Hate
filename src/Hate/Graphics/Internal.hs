{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hate.Graphics.Internal where

import Hate.Common.Types
import Hate.Graphics.Types

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U

import Control.Monad.State
import Control.Applicative

import Data.Vect.Float
import Data.List
import Data.Ord

instance MonadState GraphicsState (HateDraw us) where
    get = HateDraw $ graphicsState <$> gets libraryState
    put x = HateDraw $ do
        g <- get
        let ls = libraryState g
        put $ g { libraryState = ls { graphicsState = x } }

type Action a = forall us. HateDraw us a

fromVertArrayInto :: [Vec2] -> VertexStream -> Action VertexStream
fromVertArrayInto verts m = HateDraw $ liftIO $ do
    GL.bindBuffer GL.ArrayBuffer $= Just (vbo m)
    U.replaceBuffer GL.ArrayBuffer verts
    
    -- fill in texture coordinates
    let texCoords = calculateTexCoords verts
    GL.bindBuffer GL.ArrayBuffer $= Just (texVbo m)
    U.replaceBuffer GL.ArrayBuffer texCoords
    
    return $ m { vertNum = length verts }

fromVertArrayIntoGlobal :: [Vec2] -> Action ()
fromVertArrayIntoGlobal xs = do
    m <- gets globalVertexStream
    m' <- fromVertArrayInto xs m
    modify $ \x -> x { globalVertexStream = m' }

calculateTexCoords :: [Vec2] -> [Vec2]
calculateTexCoords verts = map (flipY . pointwise scaleFactor) verts
    where
        maxX = _1 $ maximumBy (comparing _1) verts
        maxY = _2 $ maximumBy (comparing _2) verts
        scaleFactor = Vec2 (1 / maxX) (1 / maxY)
        flipY (Vec2 x y) = Vec2 x (1 - y)

