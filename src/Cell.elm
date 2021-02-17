module Cell exposing (Cell(..), CellId, Cells, getCellAtPosition, isAtPosition)

import Position exposing (Position, X, Y)


type alias CellId =
    Int


type alias CellData =
    { id : CellId
    , pos : Position
    }


type Cell
    = Cell CellData
    | EmptyCell


type alias Cells =
    List Cell


getCellAtPosition : Cells -> Position -> Cell
getCellAtPosition cells pos =
    let
        cell = List.filter (isAtPosition pos) cells |> List.head
    in
    case cell of
        Just realCell -> realCell
        Nothing -> EmptyCell

isAtPosition : Position -> Cell -> Bool
isAtPosition pos cell =
    case cell of
        Cell cellPos -> Position.equal cellPos.pos pos
        EmptyCell -> False
