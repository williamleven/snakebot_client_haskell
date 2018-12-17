{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Handler where
-- Project datatypes
import Messages
import Types
-- Other imports
import Text.Printf (printf)

-- | Returns a tuple of actions to perform for the specefied incomming message
-- | Tuple contains new state, possible print and a possible return message
-- | The handler also thakes a satte and a update functions for the bot
handleMessage :: UpdateFunction a -> a -> InboundMessage -> 
    (a, Maybe String, Maybe OutboundMessage)
-- | Game Tick (Map Update)
handleMessage uf state (
    MapUpdate MapUpdateData{
        map, 
        gameId, 
        gameTick, 
        receivingPlayerId
}) = (newState, Just log, Just message)
    where
        (dir, newState) = uf map state
        message = RegisterMove 
            $ RegisterMoveData dir gameTick gameId receivingPlayerId
        log = printf "You did make a %s turn.\n" (show dir)

-- | Game Started
handleMessage _ state (
    GameStarting GameStartingData{
        gameId, 
        noofPlayers, 
        width, 
        height, 
        ..
}) = (state, Just m, Nothing)
    where
        m = printf "Game %s started with %d players on a %d x %d map.\n" 
            gameId noofPlayers width height

-- | Game Ended
handleMessage _ state (
    GameEnded GameEndedData{
        gameId, 
        receivingPlayerId, 
        playerWinnerId
}) = (state, Just m, Nothing)
    where
        m = printf "Game %s ended, you %s.\n" 
            gameId (choose (receivingPlayerId == playerWinnerId) "won" "lost")
        choose True  x _ = x
        choose False _ x = x

-- | Game Result
handleMessage _ state (
    GameResultMsg GameResultMsgData{
        gameId, 
        timestamp, 
        playerRanks, 
        ..
}) = (state, Just m, Nothing)
    where
        m = printf "Game %s result TODO\n" gameId

-- | Game Link
handleMessage _ state (
    GameLink GameLinkData{
        gameId, 
        url, 
        ..
}) = (state, Just m, Nothing)
    where
        m = printf "Watch game %s here: %s\n" gameId url

-- | Snake Died
handleMessage _ state (
    SnakeDead SnakeDeadData{
        playerId, 
        deathReason, 
        gameId, 
        gameTick
}) = (state, Just m, Nothing)
    where 
        m = printf "Snake %s died in game %s on tick %d due to %s.\n" 
            playerId gameId gameTick deathReason

-- | Tournament ended
handleMessage _ state (
    TournamentEnded TournamentEndedData{
        playerWinnerId, 
        gameId, 
        gameResult, 
        tournamentId, 
        tournamentName
}) = (state, Just m, Nothing)
    where
        m = printf "Tournament %s (%s) ended with game %s, winner was %s.\n" 
            tournamentName tournamentName gameId playerWinnerId

-- | Response to heartbeat
handleMessage _ state (HeartBeatResponse HeartBeatData{..}) = 
    (state, Nothing, Nothing)

-- | No action defined for message (debugging purposes)
handleMessage _ state _ = 
    (state, Just "Message not recognized", Nothing)