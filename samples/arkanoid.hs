{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}
 
import qualified Graphics.Rendering.OpenGL as GL -- Vertex2/Float
import Hate
import Control.Lens
import Control.Applicative((<$>), (<*>))
import Data.Maybe (catMaybes)
import qualified Graphics.UI.GLFW as GLFW (Key(..)) 
 
data Player = Player { _playerSpeed :: Float, _playerPos :: Float, _playerLives :: Int, _playerInst :: Instance }
data Block = Block { _blockInst :: Instance } -- TODO: colours?
data Ball = Ball { _ballPos :: GL.Vertex2 GL.GLfloat, _ballVelocity :: GL.Vertex2 GL.GLfloat, _ballInst :: Instance }
data Arkanoid = Arkanoid { _player :: Player, _blocks :: [Maybe Block], _ball :: Ball }
makeFields ''Player
makeFields ''Block
makeFields ''Ball
makeLenses ''Arkanoid
 
instance Hate.Drawable Player where
    draw p = draw $ _playerInst p 
 
instance Hate.Drawable Ball where
    draw b = draw $ _ballInst b
 
-- some constants
mapSize :: Integral i => (i,i)
mapSize = (10,10) 
blockSize = (50,20)
 
-- helpers
createBox xsize ysize = [0.0, 0.0, xsize, 0.0, xsize, ysize, 0.0, ysize]
createBoxMesh xsize ysize = fromVertArray $ createBox xsize ysize
origin = GL.Vertex2 0.0 0.0
vertex (x, y) = GL.Vertex2 (realToFrac x) (realToFrac y)
 
x :: Functor f => (GL.GLfloat -> f GL.GLfloat) -> GL.Vertex2 GL.GLfloat -> f (GL.Vertex2 GL.GLfloat)
x f (GL.Vertex2 vx vy) = fmap (\x' -> GL.Vertex2 x' vy) (f vx)
y :: Functor f => (GL.GLfloat -> f GL.GLfloat) -> GL.Vertex2 GL.GLfloat -> f (GL.Vertex2 GL.GLfloat)
y f (GL.Vertex2 vx vy) = fmap (\y' -> GL.Vertex2 vx y') (f vy)
 
a +~= b = do
    b' <- b
    a += b'
 
-- logic
checkWallCollisions :: Ball -> Ball
checkWallCollisions ball 
    | bx `outside` (0,800) = ball & velocity.x %~ negate
    | by `outside` (0,600) = ball & velocity.y %~ negate
    | otherwise = ball
    where outside a (min,max) = (a < min) || (a > max)
          bx = ball^.pos.x
          by = ball^.pos.y
 
checkBlockCollisions :: [Maybe Block] -> Ball -> Ball
checkBlockCollisions blocks ball =
    -- algorithm:
    -- If it collides with a block, basing on the speed vector
    -- find possible entry walls. Decide between two of them
    -- by comparing the (corner-point) vector with the speed
    -- vector. Bounce appropriately.
 
 
    case blockUnderBall of
        Just (Block _) -> checkBlock
        Nothing -> ball -- return unmodified ball
    where checkBlock
           | sx <= 0 && sy <= 0 = checkBottomRight
           | sx <= 0 && sy > 0  = checkTopRight
           | sx > 0  && sy <= 0 = checkBottomLeft
           | sx > 0  && sy > 0  = checkTopLeft
           where  
                 blockCoord = worldSpaceToBlockSpace (px, py)

                 blockLeft = fst blockCoord
                 blockRight = blockLeft + fst blockSize
                 blockTop = snd blockCoord
                 blockBottom = blockTop + snd blockSize
 
                 -- this gets the tangent of the line crossing
                 -- chosen corner and ball position
                 getD va vb = (fst vb - fst va) / (snd vb - snd va)
                 -- this compares the tangent of speed with the tangent
                 -- calculated above and decides on bounce direction
                 decide wa wb = if (getD (wa, wb) (px, py)) > (sx / sy) then bounceY else bounceX
                    
                 checkBottomRight = decide blockRight blockBottom
                 checkBottomLeft = decide blockLeft blockBottom 
                 checkTopRight = decide blockRight blockTop
                 checkTopLeft = decide blockLeft blockTop

                 bounceX = ball & velocity.x %~ negate
                 bounceY = ball & velocity.y %~ negate

          px = realToFrac $ ball ^. pos.x
          py = realToFrac $ ball ^. pos.y
          sx = realToFrac $ ball ^. velocity.x
          sy = realToFrac $ ball ^. velocity.y

          blockSizeXf = realToFrac . fst $ blockSize
          blockSizeYf = realToFrac . fst $ blockSize
          worldSpaceToBlockSpace (wx, wy) = ( fromIntegral . floor $ (wx / blockSizeXf), fromIntegral . floor $ (wy / blockSizeYf) )

          blockSpaceToBlockIndex (bx,by) = bx + by * (snd $ mapSize)
          blockUnderPos p = blocks !! index
                            where bsCoord = worldSpaceToBlockSpace $ p
                                  index | outside = 10000
                                        | otherwise = blockSpaceToBlockIndex bsCoord
                                  outside = any id [ 
                                                 fst bsCoord < 0,
                                                 fst bsCoord >= fst mapSize,
                                                 snd bsCoord < 0,
                                                 snd bsCoord >= snd mapSize
                                                ]
                                            
          blockUnderBall = blockUnderPos (px, py)
 
sampleLoad :: LoadFn Arkanoid
sampleLoad = do
    pipeline <- createPipeline "shader.vert" "shader.frag"
    boxMesh <- createBoxMesh (fst blockSize) (snd blockSize)
    playerMesh <- createBoxMesh 100 20
    ballMesh <- createBoxMesh 10 10
 
    let playerInstance = Instance playerMesh pipeline (vertex (400, 500))
        playerObject = Player 0 0 3 playerInstance
 
        mapInsts = blockInstance <$> [1..(snd mapSize)] <*> [1..(fst mapSize)]
             where blockInstance y x = Instance boxMesh pipeline (pos x y)
                   pos x y = vertex (x * fst blockSize, y * snd blockSize)
        blocksObject = map (Just . Block) mapInsts
 
        ballInstance = Instance ballMesh pipeline (vertex (450, 490))
        ballObject = Ball (vertex (450, 490)) (vertex (1.6, -4.0)) ballInstance
 
        arkanoid = Arkanoid playerObject blocksObject ballObject
 
    return arkanoid
 
 
sampleDraw :: DrawFn Arkanoid
sampleDraw = do 
    rblocks <- use blocks
    let blockInsts = map (view inst) (catMaybes rblocks)
    mapM_ glishaDraw blockInsts

    use player >>= glishaDraw
    use ball >>= glishaDraw
 
    let when' b a = do
        b' <- b
        when b' a
 
    let whenK k a = when' (glishaGetKey k) a
--    let x = element 0
 
{-
    whenK GLFW.Key'Right $ player.inst.position.(element 0) += 0.01
    whenK GLFW.Key'Left $ player.inst.position.(element 0) -= 0.01
    whenK GLFW.Key'Up $ player.inst.position.(element 1) += 0.01
    whenK GLFW.Key'Down $ player.inst.position.(element 1) -= 0.01
-}  
    whenK GLFW.Key'Right $ player.speed += 1.0
    whenK GLFW.Key'Left $ player.speed -= 1.0
 
    -- ball
    rball <- use ball
 
    ball.pos.x += rball^.velocity.x
    ball.pos.y += rball^.velocity.y
    ball.inst.position .= rball^.pos
 
    ball %= checkWallCollisions
    ball %= checkBlockCollisions rblocks
    -- player
    let pp c = player.inst.position.c
    
    s <- use (player . speed)
    pp x += realToFrac s
 
    rpx <- use $ pp x
 
    when (rpx < 0) $ do
        pp x .= 0
        player.speed .= 0
 
    when (rpx > 700) $ do
        pp x .= 700
        player.speed .= 0
    
    player.speed *= 0.95
 
 
main = runApp sampleLoad sampleDraw
