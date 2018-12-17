{-# LANGUAGE DeriveGeneric #-} -- Used to automaticly derive json parsing
{-# LANGUAGE DuplicateRecordFields #-}
module Types where
import Data.Aeson
import GHC.Generics (Generic)
import Numeric.Natural
import Text.Printf

-- | Type aliases to avoid confusing type signatures
type UpdateFunction a = Map -> a -> (Direction, a)
type Position = Natural
type Coordinate = (Integer, Integer)

-- | Represents the users implementation of a snakebot
data SnakeBot a = SnakeBot
    { iHost    :: String
    , iPort    :: Int
    , iPath    :: String
    , iName    :: String
    , iUpdate  :: UpdateFunction a
    , iState   :: a
    }

-- | Enum representation of user move
data Direction = RIGHT | DOWN | LEFT | UP
    deriving (Eq, Enum, Show, Generic)
instance ToJSON Direction

-- | Data representation of Snake
data SnakeInfo = SnakeInfo {
    id        :: String,
    name      :: String,
    points    :: Integer,
    positions :: [Position],
    tailProtectedForGameTicks :: Natural
} deriving (Show, Generic, Eq)
instance ToJSON SnakeInfo
instance FromJSON SnakeInfo

-- | Data representation of map
data Map = Map {
    width             :: Natural,
    height            :: Natural,
    worldTick         :: Natural,
    snakeInfos        :: [SnakeInfo],
    foodPositions     :: [Position],
    obstaclePositions :: [Position]
} deriving (Show, Generic)
instance ToJSON Map
instance FromJSON Map

-- | Data represnetation of a map tile
data Tile = Wall
          | Food
          | Obstacle
          | Empty
          | SnakeHead SnakeInfo
          | SnakeBody SnakeInfo
          | SnakeTail SnakeInfo
    deriving (Show, Eq)

-- | Represnetation of a game result
data GameResult = GameResult {
    name     :: String,
    points   :: Integer,
    playerId :: String
} deriving (Show, Generic)
instance ToJSON GameResult
instance FromJSON GameResult

-- | Representation of the game settings in tournaments
data GameSettings = GameSettings {
    maxNoofPlayers       :: Natural,
    startSnakeLength     :: Natural,
    timeInMsPerTick      :: Natural,
    obstaclesEnabled     :: Bool,
    foodEnabled          :: Bool,
    headToTailConsumes   :: Bool,
    tailConsumeGrows     :: Bool,
    addFoodLikelihood    :: Natural,
    removeFoodLikelihood :: Natural,
    spontaneousGrowthEveryNWorldTick   :: Natural,
    trainingGame         :: Bool,
    pointsPerLength      :: Natural,
    pointsPerFood        :: Natural,
    pointsPerCausedDeath :: Natural,
    pointsPerNibble      :: Natural,
    noofRoundsTailProtectedAfterNibble :: Natural
} deriving (Show, Generic)
instance ToJSON GameSettings
instance FromJSON GameSettings
