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
--import Data.List
--import Data.Ord

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
    return $ m { vertNum = length verts }

fromVertArrayIntoGlobal :: [Vec2] -> Action ()
fromVertArrayIntoGlobal xs = do
    m <- gets globalVertexStream
    m' <- fromVertArrayInto xs m
    modify $ \x -> x { globalVertexStream = m' }

{-
fromVertArrayIntoGlobalTex :: [Float] -> Action ()
fromVertArrayIntoGlobalTex xs = do
    m <- gets globalMesh
    m' <- fromVertArrayInto xs m
    modify $ \x -> x { globalMesh = m' }
-}


{-
    where -- texturing-related computations
          maxX = _1 $ maximumBy (comparing _1) verts
          maxY = _2 $ maximumBy (comparing _2) verts
          scaleFactor = Vec2 (1 / maxX) (1 / maxY)

          texCoords = map (pointwise scaleFactor) verts
          rawTexCoords = map realToFrac . concat . map unpackVec $ texCoords
-}

{-
sprite = 
    render (Sprite (sx, sy) tex) = do
        Haterender $ liftIO $ do
            GL.activeTexture $= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D $= Just tex
        let fsx = fromIntegral sx
            fsy = fromIntegral sy
        render $ Polygon [Vec2 0 0, Vec2 fsx 0, Vec2 fsx fsy, Vec2 0 fsy]

-}
