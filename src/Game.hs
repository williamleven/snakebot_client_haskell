{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Game (
    startGame
) where 
-- Project datatypes and functions
import Messages
import Types
import Handler (handleMessage)
-- Other imports
import System.Info (os, compilerVersion, compilerName)
import Data.Version (showVersion)
import Control.Concurrent (forkIO, threadDelay)
import Data.Text (unpack, pack)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy (putStrLn)
import Data.Maybe
import qualified Network.WebSockets as WS

-- | Entrypoint for the snakebot, this starts a client with the specified bot 
startGame :: SnakeBot a -> IO ()
startGame s = WS.runClient (iHost s) (iPort s) (iPath s) (client s)

-- | The actual snakebot client
-- | Handles setup of the game and starts the necessary loops.
client :: SnakeBot a -> WS.Connection -> IO ()
client s wsc = do
    -- Register player
    WS.sendTextData wsc 
        $ encode (RegisterPlayer (RegisterPlayerData (iName s)))

    -- Wait for response and extract player id
    msg <- WS.receiveData wsc
    let registerResponse = decode msg :: Maybe InboundMessage
    let pId = playerId registerResponse

    if isNothing pId then
        error "Failed to register" 
    else do
        -- Send Client info
        WS.sendTextData wsc $ encode $ ClientInfo clientInfo

        -- Start heart beat on separate thread
        io <- forkIO $ sendHeartBeat (fromJust pId) wsc

        -- Game event Loop
        readMessages (iUpdate s) (iState s) wsc
    where
        -- | Extracts the player id from register response
        playerId (Just (PlayerRegistered PlayerRegisteredData{
                receivingPlayerId, 
                ..
        })) = 
            Just receivingPlayerId
        playerId _ = Nothing
        -- | Info about bot client
        clientInfo = ClientInfoData 
            "haskell" 
            (compilerName ++ "-" ++ showVersion compilerVersion) 
            os
            "???"
            "1.0.0"
            
-- | Loop used to sent heartbeat to server
sendHeartBeat :: String -> WS.Connection -> IO ()
sendHeartBeat id wsc = do
    WS.sendTextData wsc $ encode (HeartBeatRequest (HeartBeatData id))
    threadDelay $ 1 * 1000 * 1000
    sendHeartBeat id wsc

-- | Loop used to process incomming messages
readMessages :: UpdateFunction a -> a -> WS.Connection -> IO ()
readMessages uf state wsc = do
    msg <- WS.receiveData wsc :: IO Lazy.ByteString
    let decoded = decode msg :: Maybe InboundMessage

    if isNothing decoded then do
        putStr "Failed to decode message:" 
        Lazy.putStrLn msg
        readMessages uf state wsc
    else do
        -- Calculate actions based on the incomming mesage
        let (newState, mLog, mMessage) =
                handleMessage uf state $ fromJust decoded
        printM mLog
        sendM mMessage
        readMessages uf newState wsc
    where
        printM (Just s) = putStr s
        printM _    = return ()
        sendM (Just message) = WS.sendTextData wsc $ encode message
        sendM _ = return ()
