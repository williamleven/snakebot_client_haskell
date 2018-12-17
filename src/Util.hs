{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Util (
    natCoord,
    toCoordinate,
    toPosition,
    manhattanDist,
    euclidianDist,
    isWithinSquare,
    addCoords,
    subCoords,
    dirToDelta,
    dirFromDelta,
    turnDir,
    reverseDir,
    coordAddDelta,
    insideMapBounds,
    getSnakeById,
    getSnakeByName,
    getTileAt,
    snakeFacing,
    tileSafe,
    tileSafeSnake
) where
import Prelude hiding (id, LEFT, RIGHT)
import Numeric.Natural
import Data.Maybe
import Types

{-
    This file contains helper functions that
    you can use when creating your snakebot.
-}


-- | Convert a tuple of integers to a tuple of naturals
natCoord :: Coordinate -> (Natural, Natural)
natCoord (x, y) = (fromIntegral x, fromIntegral y)

-- | Convert a position stored as a single number to (x, y).
-- | This requires knowing the width of the map.
toCoordinate :: Position -> Natural -> Coordinate
toCoordinate p mw = (x, y)
    where y = div (p' - x) mw'
          x = mod p' mw'
          p' = toInteger p
          mw' = toInteger mw

-- | Convert (x, y) to a position stored as a single number.
-- | This requires knowing the width of the map.
toPosition :: Coordinate -> Natural -> Position
toPosition c mapWidth = x + (y * mapWidth)
    where (x, y) = natCoord c

-- | Calculate the manhattan distance between two coordinates
manhattanDist :: Coordinate -> Coordinate -> Natural
manhattanDist (x1, y1) (x2, y2) = fromIntegral $ xd + yd
    where xd = abs $ x1 - x2
          yd = abs $ y1 - y2

-- | Calculate the euclidian distance between two coordinates
euclidianDist :: Coordinate -> Coordinate -> Float
euclidianDist (x1, y1) (x2, y2) = sqrt ((xd ** 2) + (yd ** 2))
    where xd = fromIntegral $ abs $ x1 - x2
          yd = fromIntegral $ abs $ y1 - y2

-- | Check whether coordinate A is within a
-- | square defined by coordinates B & C
isWithinSquare :: Coordinate -> Coordinate -> Coordinate -> Bool
isWithinSquare (x,y) (x1,y1) (x2,y2)
    = and [x >= wX, x <= eX, y >= nY, y <= sY]
    where nY = min y1 y2
          sY = max y1 y2
          wX = min x1 x2
          eX = max x1 x2

-- | Add two coordinates
addCoords :: Coordinate -> Coordinate -> Coordinate
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | Subtract coordinate B from coordinate A
subCoords :: Coordinate -> Coordinate -> Coordinate
subCoords (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)


-- | Convert a direction enum to a coordinate vector of magnitude 1
dirToDelta :: Direction -> Coordinate
dirToDelta UP    = ( 0,-1)
dirToDelta DOWN  = ( 0, 1)
dirToDelta LEFT  = (-1, 0)
dirToDelta RIGHT = ( 1, 0)

-- | Convert a coordinate vector to a direction enum.
-- | If the result is ambiguous, prefer RIGHT/LEFT over UP/DOWN.
dirFromDelta :: Coordinate -> Direction
dirFromDelta (x, y) | x' >= y' && x > 0 = RIGHT
                    | x' >= y' && x < 0 = LEFT
                    | x' <  y' && y > 0 = DOWN
                    | x' <  y' && y < 0 = UP
    where x' = abs x
          y' = abs y
dirFromDelta _ = RIGHT

-- | Turn a direction enum by n steps clockwise
-- | e.g. Up -> Right -> Down -> Left -> Up -> ..
turnDir :: Direction -> Integer -> Direction
turnDir d n = toEnum (mod (i + n') 4)
    where i = fromEnum d
          n' = fromInteger n

-- | Reverse a direction
-- | e.g. Up -> Down or Left -> Right
reverseDir :: Direction -> Direction
reverseDir d = turnDir d 2

-- | Convert a direction B to a coordinate
-- | delta and then add it to direction A.
coordAddDelta :: Coordinate -> Direction -> Coordinate
coordAddDelta c d = addCoords c $ dirToDelta d

-- | Check whether a coordinate is within map bounds
insideMapBounds :: Map -> Coordinate -> Bool
insideMapBounds Map{width, height, ..} (x, y)
    = and [x >= 0, y >= 0, x < w, y < h]
    where (w, h) = (toInteger width, toInteger height)

-- | Extract a snake from a map by its name.
-- | NOTE: Two snakes might share the same name,
-- | in this case either one may be returned.
getSnakeByName :: Map -> String -> Maybe SnakeInfo
getSnakeByName Map{snakeInfos, ..} name' =
    headMaybe $ filter byName snakeInfos
    where  headMaybe [] = Nothing
           headMaybe l = Just $ head l
           byName SnakeInfo{name, ..} = name == name'

-- | Extract a snake from a map by its unique ID.
getSnakeById :: Map -> String -> Maybe SnakeInfo
getSnakeById Map{snakeInfos, ..} id' =
    headMaybe $ filter byId snakeInfos
    where  headMaybe [] = Nothing
           headMaybe l = Just $ head l
           byId SnakeInfo{id, ..} = id == id'

getSnakeTileAt' :: SnakeInfo -> [Position] -> Position -> Maybe Tile
getSnakeTileAt' _ [] _ = Nothing
getSnakeTileAt' si [p'] p    | p == p'   = Just (SnakeTail si)
getSnakeTileAt' si (p':ps) p | p == p'   = Just (SnakeBody si)
                             | otherwise = getSnakeTileAt' si ps p

getSnakeTileAt :: SnakeInfo -> Position -> Maybe Tile
getSnakeTileAt SnakeInfo{positions = [], ..} _ = Nothing
getSnakeTileAt si p | p == p'   = Just (SnakeHead si)
                    | otherwise = getSnakeTileAt' si ps p
    where SnakeInfo{positions=(p':ps), ..} = si

-- | Get the type of a tile at a coordinate in the map.
getTileAt :: Map -> Coordinate -> Tile
getTileAt m c | not $ insideMapBounds m c = Wall
              | st:_ <- snakeTile = st
              | isObstacle = Obstacle
              | isFood     = Food
              | otherwise  = Empty
    where isObstacle = p `elem` obstaclePositions
          isFood = p `elem` foodPositions
          snakeTile = mapMaybe (`getSnakeTileAt` p) snakeInfos
          p = toPosition c width
          Map{snakeInfos, foodPositions, obstaclePositions, width, ..} = m

-- | Get the direction a specific snake:s head is facing.
snakeFacing :: SnakeInfo -> Natural -> Direction
snakeFacing SnakeInfo{positions=[]}        _ = RIGHT
snakeFacing SnakeInfo{positions=[p]}       _ = RIGHT
snakeFacing SnakeInfo{positions=(p1:p2:_)} w = dirFromDelta (subCoords c1 c2)
    where c1 = toCoordinate p1 w
          c2 = toCoordinate p2 w

-- | Check whether a tile is safe to enter by any snake
tileSafe :: Tile -> Bool
tileSafe Empty = True
tileSafe Food  = True
tileSafe _     = False

-- | Check whether a tile is safe to enter by a specific snake.
-- | This consideres the tails of other snakes as safe tiles.
tileSafeSnake :: Tile -> SnakeInfo -> Bool
tileSafeSnake (SnakeTail otherSnake) ourSnake =
    otherId /= ourId && not tailProtected
    where tailProtected = tailProtectedForGameTicks otherSnake > 0
          otherId = id otherSnake
          ourId = id ourSnake
tileSafeSnake t _ = tileSafe t
