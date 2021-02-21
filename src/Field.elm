module Field exposing (Direction(..), Field(..), FieldId(..), WarehouseFloor(..), viewField, Msg(..))

import Element exposing (Element, el, height, mouseOver, px, rgb, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Position exposing (Position)

type Msg =
    LeftClickOnField Field

type Direction
    = Up
    | Down
    | Left
    | Right


type FieldId
    = Int


type WarehouseFloor
    = Field


type alias BeltField =
    { direction : Direction
    , thing : Int
    , pos: Position
    }


type Field
    = EmptyField Position
    | WarehouseFloor Position
    | Belt BeltField


viewField : Field -> Element Msg
viewField field =
    let
        (color, pos) =
            case field of
                WarehouseFloor fieldPos ->
                    (rgb 1.0 0.0 0, fieldPos)

                Belt data ->
                    (rgb 0.0 1.0 0, data.pos)

                EmptyField fieldPos ->
                    (rgb 0.3 0.3 0.3, fieldPos)
    in
    el
        [ mouseOver
            [ Background.color (rgb 0.65 0.65 0.65)
            ]
        , Border.width 1
        , Border.solid
        , Border.color (rgb 0.25 0.25 0.25)
        , width <| px 64
        , height <| px 64
        , Background.color color
        , Element.Events.onClick <| LeftClickOnField field
        ]
        (text ("" ++ String.fromInt pos.x))
