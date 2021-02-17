module Position exposing (Position, X, Y, equal, getPositionFromInt, createPosition)


type alias X =
    Int


type alias Y =
    Int


type alias Position =
    { x : X
    , y : Y
    }


getPositionFromInt : Int -> Position
getPositionFromInt num =
    let
        y =
            floor (toFloat num / 20)

        x =
            num - (y * 20)
    in
    {x = x, y = y}


createPosition : Int -> Int -> Position
createPosition x y =
    { x = x, y = y }


equal : Position -> Position -> Bool
equal pos otherPos =
    pos.x == otherPos.x && pos.y == otherPos.y
