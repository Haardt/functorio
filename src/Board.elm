module Board exposing (Board, ViewPort, getFieldFromCell, getFieldAtPosition)

import Cell exposing (Cell(..), CellId, Cells)
import Dict exposing (Dict)
import Field exposing (Field(..))
import Position exposing (Position)


type alias Zoom =
    Int


type alias ViewPort =
    { pos : Position
    , z : Zoom
    }


type alias Board =
    { fields : Dict CellId Field
    , viewPort : ViewPort
    , cells : Cells
    }


getFieldAtPosition : Board -> Position -> Field
getFieldAtPosition board position =
    case Cell.getCellAtPosition board.cells position of
        Cell cell ->
            getFieldFromCell board (Cell cell)

        EmptyCell ->
            EmptyField


getFieldFromCell : Board -> Cell -> Field
getFieldFromCell board cell =
    case cell of
        Cell idCell ->
            Maybe.withDefault EmptyField <| Dict.get idCell.id board.fields
        EmptyCell -> EmptyField
