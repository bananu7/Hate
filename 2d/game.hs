{-# LANGUAGE TemplateHaskell #-}
 
-- game imports 
import Data.List
import Control.Lens
import Control.Monad(unless, when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans
import Control.Applicative((<$>), (<*>))
import Control.Concurrent (threadDelay)

data Point = Point { _x :: Int, _y :: Int } deriving (Eq, Show)
data Player = Player { _position :: Point } deriving (Eq, Show)
makeLenses ''Player
makeLenses ''Point
 
data Field = FieldBlocked | FieldEmpty deriving (Eq, Show) 
type Board = [[Field]]
data Game = Game { _player :: Player, _board :: Board }
makeLenses ''Game
 
boardSize = 5
emptyBoard = (replicate boardSize) . (replicate boardSize) $ FieldEmpty
newGame = Game { 
    _board = emptyBoard,
    _player = Player (Point half half)
    }
    where half = boardSize `div` 2
 
update = do
    zoom(player.position) $ do
        x += 1
        y -= 1
 
draw game = print $ _player game
 
main = do
    runStateT loop newGame where 
       loop = forever $ do
           update
           get >>= liftIO . draw
           liftIO $ threadDelay $ 1000*1000
 

