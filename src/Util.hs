{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Util where
import Prelude hiding (id, LEFT, RIGHT)
import Numeric.Natural
import Data.Maybe
import Types

-- Coordinate related functions
natCoord :: Coordinate -> (Natural, Natural)
natCoord (x, y) = (fromIntegral x, fromIntegral y)

toCoordinate :: Position -> Natural -> Coordinate
toCoordinate p mw = (x, y)
    where y = div (p' - x) mw'
          x = mod p' mw'
          p' = toInteger p
          mw' = toInteger mw

toPosition :: Coordinate -> Natural -> Position
toPosition c mapWidth = x + (y * mapWidth)
    where (x, y) = natCoord c

manhattanDist :: Coordinate -> Coordinate -> Natural
manhattanDist (x1, y1) (x2, y2) = fromIntegral $ xd + yd
    where xd = abs $ x1 - x2
          yd = abs $ y1 - y2

euclidianDist :: Coordinate -> Coordinate -> Float
euclidianDist (x1, y1) (x2, y2) = sqrt ((xd ** 2) + (yd ** 2))
    where xd = fromIntegral $ abs $ x1 - x2
          yd = fromIntegral $ abs $ y1 - y2

isWithinSquare :: Coordinate -> Coordinate -> Coordinate -> Bool
isWithinSquare (x,y) (x1,y1) (x2,y2)
    = and [x >= wX, x <= eX, y >= nY, y <= sY]
    where nY = min y1 y2
          sY = max y1 y2
          wX = min x1 x2
          eX = max x1 x2

addCoords :: Coordinate -> Coordinate -> Coordinate
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subCoords :: Coordinate -> Coordinate -> Coordinate
subCoords (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)


-- Direction releated functions
dirToDelta :: Direction -> Coordinate
dirToDelta UP    = ( 0,-1)
dirToDelta DOWN  = ( 0, 1)
dirToDelta LEFT  = (-1, 0)
dirToDelta RIGHT = ( 1, 0)

dirFromDelta :: Coordinate -> Direction
dirFromDelta (x, y) | x' >= y' && x > 0 = RIGHT
                    | x' >= y' && x < 0 = LEFT
                    | x' <  y' && y > 0 = DOWN
                    | x' <  y' && y < 0 = UP
    where x' = abs x
          y' = abs y
dirFromDelta _ = RIGHT

turnDir :: Direction -> Integer -> Direction
turnDir d n = toEnum (mod (i + n') 4)
    where i = fromEnum d
          n' = fromInteger n

reverseDir :: Direction -> Direction
reverseDir d = turnDir d 2

coordAddDelta :: Coordinate -> Direction -> Coordinate
coordAddDelta c d = addCoords c $ dirToDelta d


-- Map related functions
insideMapBounds :: Map -> Coordinate -> Bool
insideMapBounds Map{width, height, ..} (x, y)
    = and [x >= 0, y >= 0, x < w, y < h]
    where (w, h) = (toInteger width, toInteger height)

getSnakeByName :: Map -> String -> Maybe SnakeInfo
getSnakeByName Map{snakeInfos, ..} name' = headMaybe $ filter byName snakeInfos
    where  headMaybe [] = Nothing
           headMaybe l = Just $ head l
           byName SnakeInfo{name, ..} = name == name'

getSnakeById :: Map -> String -> Maybe SnakeInfo
getSnakeById Map{snakeInfos, ..} id' = headMaybe $ filter byId snakeInfos
    where  headMaybe [] = Nothing
           headMaybe l = Just $ head l
           byId SnakeInfo{id, ..} = id == id'

getSnakeTileAt' :: SnakeInfo -> [Position] -> Position -> Maybe Tile
getSnakeTileAt' _ [] _ = Nothing
getSnakeTileAt' si (p':[]) p | p == p'   = Just (SnakeTail si)
getSnakeTileAt' si (p':ps) p | p == p'   = Just (SnakeBody si)
                             | otherwise = getSnakeTileAt' si ps p

getSnakeTileAt :: SnakeInfo -> Position -> Maybe Tile
getSnakeTileAt SnakeInfo{positions = [], ..} _ = Nothing
getSnakeTileAt si p | p == p'   = Just (SnakeHead si)
                    | otherwise = getSnakeTileAt' si ps p
    where SnakeInfo{positions=(p':ps), ..} = si

getTileAt :: Map -> Coordinate -> Tile
getTileAt m c | not $ insideMapBounds m c = Wall
              | st:_ <- snakeTile = st
              | isObstacle = Obstacle
              | isFood     = Food
              | otherwise  = Empty
    where isObstacle = elem p obstaclePositions
          isFood = elem p foodPositions
          snakeTile = mapMaybe (\si -> getSnakeTileAt si p) snakeInfos
          p = toPosition c width
          Map{snakeInfos, foodPositions, obstaclePositions, width, ..} = m

snakeFacing :: SnakeInfo -> Natural -> Direction
snakeFacing SnakeInfo{positions=[]}        _ = RIGHT
snakeFacing SnakeInfo{positions=(p:[])}    _ = RIGHT
snakeFacing SnakeInfo{positions=(p1:p2:_)} w = dirFromDelta (subCoords c1 c2)
    where c1 = toCoordinate p1 w
          c2 = toCoordinate p2 w

tileSafe :: Tile -> Bool
tileSafe Empty = True
tileSafe Food  = True
tileSafe (SnakeTail SnakeInfo{
        tailProtectedForGameTicks, ..
    }) |tailProtectedForGameTicks == 0 = True
tileSafe _     = False
