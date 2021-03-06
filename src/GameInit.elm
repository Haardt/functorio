module GameInit exposing (createGameModel)

import Dict
import Field exposing (Field(..))
import GameModel exposing (Game)
import Position exposing (createPosition)


createGameModel : String -> Game
createGameModel playerName =
    { board =
        { fields = Dict.singleton "66" <| WarehouseFloor <| createPosition 6 6
        , viewPort =
            { pos =
                { x = 0
                , y = 0
                }
            , z = 1
            }
        }
    , playerName = playerName
    }
