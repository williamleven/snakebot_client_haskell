# Cygni SnakeBot Client
<<<<<<< Updated upstream
![Build status](https://travis-ci.org/Gurgy/snakebot_client_haskell.svg?branch=master)

A Haskell client for cygnis copettative snakebot game. This client allows the user to write their own implementetation of a snakebot and run it on the [cygni snakebot server](https://github.com/cygni/snakebot).  
=======
A Haskell client for cygnis competitive snakebot game. This client allows the user to write their own implementation of a snakebot and run it on the [cygni snakebot server](https://github.com/cygni/snakebot).  
>>>>>>> Stashed changes

## Usage 
This project uses [stack](https://docs.haskellstack.org/en/stable/README/). 

`stack run` will install all dependencies and run the client.

You provide your implementation of the bot in `app/Main.hs` by implementing the update function.
```haskell
update :: Map -> State -> (Direction, State)
update _ state = (UP, state)
```

To change the name of your bot (REQUIRED) and other settings you edit the myBot definition.
```haskell
myBot = SnakeBot
  { iHost  = "snake.cygni.se"
  , iPort  = 80
  , iPath  = "/training"
  , iName = "My haskell bot"
  , iUpdate = update
  , iState = State
    { whateverYouWant = "initial state"
    , whatever        = Nothing
    }
  }
```

If you want to have a persistent state between calls to update you can define your own state.
```haskell
data State = State 
  { whateverYouWant :: String
  , whatever        :: Maybe String
}
```
If you do so, remember to provide initial values in your snakebot definition.
```haskell
myBot = SnakeBot
  { 
    ...
  , iState = State
    { whateverYouWant = "initial state"
    , whatever        = Nothing
    }
  }
```

There are multiple util methods in `src/Util.hs` that can help you analyze the map.

## Development
Please read through the usage section to get a better understanding of how the user sees the application.

Use `stack ghci` to run gchi with stack.

Use `stack test` to run all tests.

* `Game.hs` takes care about client setup and the websocket loop for reading and sending messages.
* `Handler.hs` takes care about calculating appropriate action to each incoming message in the game loop.
* `Types.hs` contains types that the user use in the update function.
* `Util.hs` helper methods to be used by the user, these should have tests.
* `Messages.hs` Message datatypes and json encoding/decoding.

### Json encoding / decoding
The aeson libary is used for json encoding / decoding. See the `src/Messages.hs` file for more information on how the custom type property is used.
