module GameInit exposing (createGameModel)

import Cell exposing (Cell(..))
import Dict
import Field exposing (Field(..))
import GameModel exposing (Game)
import Position exposing (createPosition)


createGameModel : String -> Game
createGameModel playerName =
    { board =
        { fields = Dict.singleton 66 <| WarehouseFloor <| createPosition 6 6
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
