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
update m state | tileSafeSnake front s = (f, state)
               | tileSafeSnake left s  = (fl, state)
               | otherwise = (fr, state)
    where s = fromJust $ getSnakeByName m (iName myBot)
          front = getTileAt m (addCoords hc fd)
          left = getTileAt m (addCoords hc ld)
          right = getTileAt m (addCoords hc rd)
          fd = dirToDelta f
          ld = dirToDelta fl
          rd = dirToDelta fr
          fl = turnDir f (-1)
          fr = turnDir f   1
          f = snakeFacing s mw
          mw = width m
          hc = toCoordinate hp mw
          SnakeInfo{positions=(hp:_)} = s

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
