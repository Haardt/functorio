module GameInit exposing (Game, createGameModel)

import Board exposing (Board)
import Cell exposing (Cell(..))
import Dict
import Field exposing (Field(..))


type alias Game =
    { board : Board
    , playerName : String
    }


createGameModel : String -> Game
createGameModel playerName =
    { board =
        { fields = Dict.singleton 66 WarehouseFloor
        , viewPort =
            { pos =
                { x = 0
                , y = 0
                }
            , z = 1
            }
        , cells = [ Cell { id = 66, pos = { x = 8, y = 8 } } ]
        }
    , playerName = playerName
    }
