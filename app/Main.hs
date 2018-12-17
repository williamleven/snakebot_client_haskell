module Main where
import Game
import Types
import Util
import Data.Maybe

data State = State 
  { whateverYouWant :: String
  , whatever        :: Maybe String
}

update :: Map -> State -> (Direction, State)
update _ state = (UP, state)

myBot = SnakeBot
  { iHost             = "snake.cygni.se"
  , iPort             = 80
  , iPath             = "/training"
  , iName             = "First haskell bot"
  , iUpdate           = update
  , iState            = State
    { whateverYouWant = "initial state"
    , whatever        = Nothing
    }
  }

main :: IO ()
main = startGame myBot
