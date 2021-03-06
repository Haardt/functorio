module Fields.Neighbour exposing (createNeighbourId, getFieldIds)

import Field exposing (FieldId)
import Position exposing (Position)


getFieldIds : Position -> List FieldId
getFieldIds pos =
    [ createNeighbourId pos.x (pos.y - 1)
    , createNeighbourId (pos.x + 1) pos.y
    , createNeighbourId (pos.x - 1) pos.y
    , createNeighbourId pos.x (pos.y + 1)
    ]


createNeighbourId : Int -> Int -> FieldId
createNeighbourId x y =
    String.fromInt x
        ++ String.fromInt y
