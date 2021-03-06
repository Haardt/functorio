module Field exposing (Field(..), FieldId, Msg(..), createFieldId, fieldSize, isBelt, viewField, getFieldId)

import Basics
import Element exposing (Color, Element, column, el, height, mouseOver, px, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Fields.Belt as Belt exposing (BeltField, BeltType(..))
import Position exposing (Position, createPosition)


type Msg
    = LeftClickOnField Field

fieldSize =
    64

type Field
    = WarehouseFloor Position
    | Belt BeltType

type alias FieldId =
    String


createFieldId : Int -> Int -> FieldId
createFieldId x y =
    String.fromInt x
        ++ String.fromInt y

getFieldId : BeltType -> FieldId
getFieldId beltType =
    let
        x =
            case beltType of
                BeltNorth item beltField -> beltField.pos.x
        y =
            case beltType of
                BeltNorth item beltField -> beltField.pos.y

    in
    String.fromInt x
        ++ String.fromInt y

type alias Item =
    { squareSize : Int
    }


isBelt : Field -> Bool
isBelt field =
    case field of
        Belt _ ->
            True
        _ ->
            False

viewField : Field -> Element Msg
viewField field =
    case field of
        WarehouseFloor x ->
            viewStandardField (rgb 1.0 0.0 0) (WarehouseFloor x)

        Belt beltType ->
            Belt.viewBelt beltType


viewStandardField : Color -> Field -> Element Msg
viewStandardField color field =
    el
        [ mouseOver
            [ Background.color (rgb 0.65 0.65 0.65)
            ]
        , Border.width 1
        , Border.solid
        , Border.color (rgb 0.25 0.25 0.25)
        , width <| px fieldSize
        , height <| px fieldSize
        , Background.color color
        , Element.Events.onClick <| LeftClickOnField field
        ]
        (let
            pos = case field of
                WarehouseFloor position -> position


                Belt beltType -> createPosition 0 0


        in
        (text <| String.fromInt pos.x ++ ":"  ++ String.fromInt pos.y)
        )
