{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hate.Graphics.Backend.Util where

--import qualified Codec.Picture as JP
--import Data.Vector.Storable (unsafeWith)

import Hate.Graphics.Types
import Hate.Graphics.Shader
import Hate.Math

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString.Char8 as BS (ByteString)

import Data.List (maximumBy)
import Data.Ord


-- for GLUtil
import Foreign.Ptr (Ptr, wordPtrToPtr)
import Foreign.Storable
import Data.Array.Storable
-- GLUTIL THINGS
-- ###########################################################################
-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

-- |Allocate and fill a 'BufferObject' from a list of 'Storable's.
makeBuffer :: Storable a => GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = makeBufferLen target (length elems) elems

-- |Allocate and fill a 'BufferObject' from a list of 'Storable's
-- whose length is explicitly given. This is useful when the list is
-- of known length, as it avoids a traversal to find the length.
makeBufferLen :: forall a. Storable a => 
                 GL.BufferTarget -> Int -> [a] -> IO GL.BufferObject
makeBufferLen target len elems = 
    do [buffer] <- GL.genObjectNames 1
       GL.bindBuffer target $= Just buffer
       let n = fromIntegral $ len * sizeOf (undefined::a)
       arr <- newListArray (0, len - 1) elems
       withStorableArray arr $ \ptr -> 
          GL.bufferData target $= (n, ptr, GL.StaticDraw)
       return buffer


-- |@replaceBuffer target elements@ replaces the buffer data attached
-- to the buffer object currently bound to @target@ with the supplied
-- list. Any previous data is deleted.
replaceBuffer :: forall a. Storable a => GL.BufferTarget -> [a] -> IO ()
replaceBuffer target elems = do arr <- newListArray (0, len - 1) elems
                                withStorableArray arr $ \ptr ->
                                  GL.bufferData target $= (n, ptr, GL.StaticDraw)
    where len = length elems
          n = fromIntegral $ len * sizeOf (undefined::a)
-- ###########################################################################
-- VECT.OPENGL THINGS

-- | \"Orthogonal projecton\" matrix, a la OpenGL 
-- (the corresponding functionality is removed in OpenGL 3.1)
orthoMatrix 
  :: (Float,Float)   -- ^ (left,right)
  -> (Float,Float)   -- ^ (bottom,top)
  -> (Float,Float)   -- ^ (near,far)
  -> Mat4 
orthoMatrix (l,r) (b,t) (n,f) = Mat4
  (Vec4 (2/(r-l)) 0 0 0)
  (Vec4 0 (2/(t-b)) 0 0)
  (Vec4 0 0 (-2/(f-n)) 0)
  (Vec4 (-(r+l)/(r-l)) (-(t+b)/(t-b)) (-(f+n)/(f-n)) 1)


toOpenGLVertex :: Vec4 -> GL.Vertex4 Float
toOpenGLVertex (Vec4 x y z w) = GL.Vertex4 x y z w
-- ###########################################################################

createVertexStream :: IO VertexStream
createVertexStream = do
    _vao <- (GL.genObjectName :: IO GL.VertexArrayObject)
    _vbo <- makeBuffer GL.ArrayBuffer ([] :: [Vec2])
    _texVbo <- makeBuffer GL.ArrayBuffer ([] :: [Vec2])
    let _vertNum = 0

    GL.bindVertexArrayObject $= Just _vao
    GL.bindBuffer GL.ArrayBuffer $= (Just _vbo)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 offset0)

    GL.bindBuffer GL.ArrayBuffer $= (Just _texVbo)
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 offset0)

    return $ VertexStream { vao = _vao, vbo = _vbo, texVbo = _texVbo, vertNum = _vertNum }

calculateTexCoords :: [Vec2] -> [Vec2]
calculateTexCoords verts = map (flipY . pointwise scaleFactor) verts
    where
        maxX = _1 $ maximumBy (comparing _1) verts
        maxY = _2 $ maximumBy (comparing _2) verts
        scaleFactor = Vec2 (1 / maxX) (1 / maxY)
        flipY (Vec2 x y) = Vec2 x y

vertexLayoutToGLLayout :: VertexLayout -> GL.PrimitiveMode
vertexLayoutToGLLayout FanVertexLayout = GL.TriangleFan
vertexLayoutToGLLayout StripVertexLayout = GL.TriangleStrip
vertexLayoutToGLLayout LinesVertexLayout = GL.Lines

type ShaderSource = BS.ByteString

-- global, shared pipeline things
globalShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderDesc
globalShader = ShaderDesc MediumPrecision

globalVertexInputs :: [Input]
globalVertexInputs =
    [ Input Vec2Tag (Just $ Location 0) "position"
    , Input Vec2Tag (Just $ Location 1) "texcoord"
    ]

globalVertexUniforms :: [Uniform]
globalVertexUniforms = [Uniform Mat4Tag Nothing "screen_transformation"]

globalVertexShader :: [Input] -> [Output] -> [Uniform] -> String -> ShaderDesc
globalVertexShader i o u s = globalShader
    (globalVertexInputs ++ i)
    o
    (globalVertexUniforms ++ u)
    s

globalFragmentOutputs :: [Output]
globalFragmentOutputs = [Output Vec4Tag "color"]

globalFragmentUniforms :: [Uniform]
globalFragmentUniforms = [] -- TODO: add time

globalFragmentShader :: [Input] -> [Uniform] -> String -> ShaderDesc
globalFragmentShader i u s = globalShader
    i
    globalFragmentOutputs
    (globalFragmentUniforms ++ u)
    s

makeGlobalPipelineDescs :: [Input] -> [Uniform] -> [Varying] -> [Uniform] -> String -> String -> (ShaderDesc, ShaderDesc)
makeGlobalPipelineDescs vertexInputs vertexUniforms varyings fragmentUniforms vss fss =
    ( globalVertexShader vertexInputs (map toOutput varyings) vertexUniforms vss
    , globalFragmentShader (map toInput varyings) fragmentUniforms fss
    )

solidColorPipelineDescs :: (ShaderDesc, ShaderDesc)
solidColorPipelineDescs = makeGlobalPipelineDescs [] [] []
    [Uniform Vec4Tag Nothing "in_color"]
    vss fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ]

        fss = "    color = in_color;"

texturingPipelineDescs :: (ShaderDesc, ShaderDesc)
texturingPipelineDescs = makeGlobalPipelineDescs
    [] -- no additional vertex inputs
    [] -- no additional vertex uniforms
    [ Varying Vec2Tag "var_position"
    , Varying Vec2Tag "var_texcoord"
    ]
    [Uniform Sampler2DTag Nothing "mainTexture"] -- our sprite texture
    vss
    fss
    where
        vss = unlines
            ["    gl_Position = screen_transformation * vec4(position, 0, 1);"
            ,"    var_position = position / 10;"
            ,"    var_texcoord = texcoord;"
            ]
        fss = "    color = texture(mainTexture, var_texcoord);"
