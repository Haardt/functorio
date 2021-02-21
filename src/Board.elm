module Board exposing (Board, Msg, ViewPort, getFieldAtPosition, getFieldFromCell, updateBoard, viewBoard)

import Cell exposing (Cell(..), CellId, Cells)
import Dict exposing (Dict)
import Element exposing (Element)
import Field exposing (Field(..), Msg(..))
import Position exposing (Position, createPosition, getPositionFromInt)


type Msg
    = FieldMsg Field.Msg


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


updateBoard : Msg -> Board -> ( Board, Cmd Msg )
updateBoard msg model =
    case msg of
        FieldMsg field ->
            case field of
                LeftClickOnField fieldType ->
                    case fieldType of
                        WarehouseFloor pos ->
                            ( model, Cmd.none )

                        EmptyField pos ->
                            let
                                id = Maybe.withDefault 0 <| String.toInt <| String.fromInt pos.x ++ String.fromInt pos.y
                                newModel =
                                    { model
                                        | fields =
                                            Dict.insert id (WarehouseFloor pos) model.fields
                                        , cells = model.cells ++ [Cell {id = id, pos = pos}]
                                    }
                            in
                            ( newModel, Cmd.none )

                        Belt data ->
                            ( model, Cmd.none )


viewBoard : Board -> List (Element Msg)
viewBoard board =
    let
        length =
            List.range 0 (20 * 12 - 1)
    in
    List.map
        (\pos ->
            getPositionFromInt pos
                |> getFieldAtPosition board
                |> Field.viewField
                |> Element.map FieldMsg
        )
        length



-- Move to field


getFieldAtPosition : Board -> Position -> Field
getFieldAtPosition board position =
    case Cell.getCellAtPosition board.cells position of
        Cell cell ->
            getFieldFromCell position board (Cell cell)

        EmptyCell ->
            EmptyField position


getFieldFromCell : Position -> Board -> Cell -> Field
getFieldFromCell pos board cell =
    case cell of
        Cell idCell ->
            Maybe.withDefault (EmptyField idCell.pos) <| Dict.get idCell.id board.fields

        EmptyCell ->
            EmptyField pos
