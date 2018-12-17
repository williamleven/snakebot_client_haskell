{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Messages where
import Types (Direction, Map, GameResult, GameSettings)
import Data.Text (pack)
import qualified Control.Applicative as CA
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import GHC.Generics (Generic)
import Numeric.Natural

{-
    This file contians datatypes for json encoding / decoding
    as well as the utilities used to do the encoding / decoding.
-}

-- | Util function to check that the object contains a specific type attribute
validType :: Object -> String -> Bool
validType o s | Just (String t') <- t
              , t' == s'
              = True
              | otherwise = False
    where t = HM.lookup (pack "type") o
          s' = pack s

-- | Util function to inject a type attribute into a json object
injectType :: ToJSON d => d -> String -> Value
injectType d t = Object $ HM.insert (pack "type") t' o
    where Object o = toJSON d
          t' = String $ pack t

-- | Data messages the client can recieve from the server
data InboundMessage = GameStarting GameStartingData
                    | GameEnded GameEndedData
                    | GameResultMsg GameResultMsgData
                    | GameLink GameLinkData
                    | HeartBeatResponse HeartBeatData
                    | InvalidPlayerName InvalidPlayerNameData
                    | MapUpdate MapUpdateData
                    | PlayerRegistered PlayerRegisteredData
                    | SnakeDead SnakeDeadData
                    | TournamentEnded TournamentEndedData
    deriving (Show, Eq)

-- | Data bessages the client can send to the server
data OutboundMessage = HeartBeatRequest HeartBeatData
                     | ClientInfo ClientInfoData
                     | RegisterPlayer RegisterPlayerData
                     | RegisterMove RegisterMoveData
                     | StartGame
    deriving (Show, Eq)

-- | Implementation for json decoding of all inbound messages
-- | The decoding is done by checking the type attribute of the object and
-- | then parsing it in the appropiate data structure.
instance FromJSON InboundMessage where
    parseJSON (Object o) | valid = GameStarting <$> parseJSON (Object o)
        where valid = validType o "se.cygni.snake.api.event.GameStartingEvent"

    parseJSON (Object o) | valid = GameEnded <$> parseJSON (Object o)
        where valid = validType o "se.cygni.snake.api.event.GameEndedEvent"

    parseJSON (Object o) | valid = GameResultMsg <$> parseJSON (Object o)
        where valid = validType o "se.cygni.snake.api.event.GameResultEvent"

    parseJSON (Object o) | valid = GameLink <$> parseJSON (Object o)
        where valid = validType o "se.cygni.snake.api.event.GameLinkEvent"

    parseJSON (Object o) | valid = HeartBeatResponse <$> parseJSON (Object o)
        where valid = validType o 
                "se.cygni.snake.api.response.HeartBeatResponse"

    parseJSON (Object o) | valid = InvalidPlayerName <$> parseJSON (Object o)
        where valid = validType o 
                "se.cygni.snake.api.exception.InvalidPlayerName"

    parseJSON (Object o) | valid = MapUpdate <$> parseJSON (Object o)
        where valid = validType o "se.cygni.snake.api.event.MapUpdateEvent"

    parseJSON (Object o) | valid = PlayerRegistered <$> parseJSON (Object o)
        where valid = validType o 
                "se.cygni.snake.api.response.PlayerRegistered"

    parseJSON (Object o) | valid = SnakeDead <$> parseJSON (Object o)
        where valid = validType o "se.cygni.snake.api.event.SnakeDeadEvent"

    parseJSON (Object o) | valid = TournamentEnded <$> parseJSON (Object o)
        where valid = validType o 
                "se.cygni.snake.api.event.TournamentEndedEvent"

    parseJSON _ = CA.empty

-- | Implementation for json encoding of all inbound messages
-- | The encoding is done my using the default aeson encoding and
-- | then injecting the type attribute.
instance ToJSON InboundMessage where
    toJSON (GameStarting d) = injectType d
        "se.cygni.snake.api.event.GameStartingEvent"

    toJSON (GameEnded d) = injectType d
        "se.cygni.snake.api.event.GameEndedEvent"

    toJSON (GameResultMsg d) = injectType d
        "se.cygni.snake.api.event.GameResultEvent"

    toJSON (GameLink d) = injectType d
        "se.cygni.snake.api.event.GameLinkEvent"

    toJSON (HeartBeatResponse d) = injectType d
        "se.cygni.snake.api.response.HeartBeatResponse"

    toJSON (InvalidPlayerName d) = injectType d
        "se.cygni.snake.api.exception.InvalidPlayerName"

    toJSON (MapUpdate d) = injectType d
        "se.cygni.snake.api.event.MapUpdateEvent"

    toJSON (PlayerRegistered d) = injectType d
        "se.cygni.snake.api.response.PlayerRegistered"

    toJSON (SnakeDead d) = injectType d
        "se.cygni.snake.api.event.SnakeDeadEvent"

    toJSON (TournamentEnded d) = injectType d
        "se.cygni.snake.api.event.TournamentEndedEvent"

-- | Implementation for json encoding of all outbound messages
-- | The encoding is done my using the default aeson encoding and
-- | then injecting the type attribute.
instance ToJSON OutboundMessage where
    toJSON (HeartBeatRequest d) = injectType d
        "se.cygni.snake.api.request.HeartBeatRequest"

    toJSON (ClientInfo d) = injectType d
        "se.cygni.snake.api.request.ClientInfo"

    toJSON (RegisterPlayer d) = injectType d
        "se.cygni.snake.api.request.RegisterPlayer"

    toJSON (RegisterMove d) = injectType d
        "se.cygni.snake.api.request.RegisterMove"

    toJSON StartGame = injectType (HM.empty :: Object)
        "se.cygni.snake.api.request.StartGame"

-- | Representation of player registered event data
newtype RegisterPlayerData = RegisterPlayerData {
    playerName :: String
} deriving (Show, Generic, Eq)
instance ToJSON RegisterPlayerData

-- | Representation of the heartbeat event data
newtype HeartBeatData = HeartBeatData {
    receivingPlayerId :: String
} deriving (Show, Generic, Eq)
instance FromJSON HeartBeatData
instance ToJSON HeartBeatData

-- | Representation of the client info event data
data ClientInfoData = ClientInfoData {
    language :: String,
    languageVersion :: String,
    operatingSystem :: String,
    operatingSystemVersion :: String,
    clientVersion :: String
} deriving (Show, Generic, Eq)
instance ToJSON ClientInfoData

-- | Representation of the player move event data
data RegisterMoveData = RegisterMoveData {
    direction :: Direction,
    gameTick :: Natural,
    gameId :: String,
    receivingPlayerId :: String
} deriving (Show, Generic, Eq)
instance ToJSON RegisterMoveData

-- | Representation of the game started event data
data GameStartingData = GameStartingData {
    gameId :: String,
    receivingPlayerId :: String,
    noofPlayers :: Natural,
    width :: Natural,
    height :: Natural
} deriving (Show, Generic, Eq)
instance FromJSON GameStartingData
instance ToJSON GameStartingData

-- | Representation of the game ended event data
data GameEndedData = GameEndedData {
    gameId :: String,
    receivingPlayerId :: String,
    playerWinnerId :: String,
    gameTick :: Natural,
    map :: Map
} deriving (Show, Generic, Eq)
instance FromJSON GameEndedData
instance ToJSON GameEndedData

-- | Representation of the game result (ROW) event data
data GameResultMsgDataRow = GameResultMsgDataRow {
    points :: Natural,
    rank :: Natural,
    alive :: Bool,
    playerId :: String,
    playerName :: String
} deriving (Show, Generic, Eq)
instance FromJSON GameResultMsgDataRow
instance ToJSON GameResultMsgDataRow

-- | Representation of the game result (ROW) event data
data GameResultMsgData = GameResultMsgData {
    gameId :: String,
    receivingPlayerId :: String,
    timestamp :: Natural,
    playerRanks :: [GameResultMsgDataRow]
} deriving (Show, Generic, Eq)
instance FromJSON GameResultMsgData
instance ToJSON GameResultMsgData

-- | Representation of the game link event data
data GameLinkData = GameLinkData {
    gameId :: String,
    receivingPlayerId :: String,
    url :: String
} deriving (Show, Generic, Eq)
instance FromJSON GameLinkData
instance ToJSON GameLinkData

-- | Representation of the invalid playername event data
newtype InvalidPlayerNameData = InvalidPlayerNameData {
    reasonCode :: Natural
} deriving (Show, Generic, Eq)
instance FromJSON InvalidPlayerNameData
instance ToJSON InvalidPlayerNameData

-- | Representation of the game tick event data
data MapUpdateData = MapUpdateData {
    gameId :: String,
    gameTick :: Natural,
    receivingPlayerId :: String,
    map :: Map
} deriving (Show, Generic, Eq)
instance FromJSON MapUpdateData
instance ToJSON MapUpdateData

-- | Representation of the registered player event data
data PlayerRegisteredData = PlayerRegisteredData {
    name :: String,
    gameId :: String,
    gameMode :: String,
    receivingPlayerId :: String,
    gameSettings :: GameSettings
} deriving (Show, Generic, Eq)
instance FromJSON PlayerRegisteredData
instance ToJSON PlayerRegisteredData

-- | Representation of the snake died event data
data SnakeDeadData = SnakeDeadData {
    playerId :: String,
    x :: Natural,
    y :: Natural,
    gameId :: String,
    gameTick :: Natural,
    deathReason :: String
} deriving (Show, Generic, Eq)
instance FromJSON SnakeDeadData
instance ToJSON SnakeDeadData

-- | Representation of the tournament ended event data
data TournamentEndedData = TournamentEndedData {
    playerWinnerId :: String,
    gameId :: String,
    gameResult :: [GameResult],
    tournamentId :: String,
    tournamentName :: String,
    receivingPlayerId :: String
} deriving (Show, Generic, Eq)
instance FromJSON TournamentEndedData
instance ToJSON TournamentEndedData


