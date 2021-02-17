module Position exposing (X, Y, equal, Position)


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    { x : X
    , y : Y
    }


equal : Position -> Position -> Bool
equal pos otherPos =
    pos.x == otherPos.x && pos.y == otherPos.y
