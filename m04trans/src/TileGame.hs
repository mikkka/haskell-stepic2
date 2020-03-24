module TileGame where 

import ExceptT
import WriterT
import StateT 
import MonadTrans
import Control.Monad

data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves map cnt start = runExceptT $ moveManyE map cnt start
  
waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie dr map cnt start = 
  length $ filter (\x -> x == Left dr) (moves map cnt start)

move1E :: GameMap -> Point -> ExceptT DeathReason [] Point
move1E map (x,y) = do
  (x',y') <- ExceptT [
    tile2e(x-1,y),
    tile2e(x+1,y),
    tile2e(x,y-1),
    tile2e(x,y+1)]
  return (x',y')
  where 
    tile2e (x,y) = case map (x,y) of
      Snake -> Left Poisoned 
      Chasm -> Left Fallen 
      Floor -> Right (x,y)

moveManyE :: GameMap -> Int -> Point -> ExceptT DeathReason [] Point
moveManyE map cnt start = 
  return start >>= foldr (<=<) return (replicate cnt $ move1E map)


map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm


