module Fields.Belt exposing (BeltField, BeltType(..), Item(..), createBeltUp, viewBelt)

import Array exposing (Array)
import Element exposing (Element, column, el, height, mouseOver, moveUp, px, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Position exposing (Position)


type BeltType
    = BeltUp (Array Item) BeltField



--| BeltDown (Array Item) BeltField
--| BeltLeft (Array Item) BeltField
--| BeltRight (Array Item) BeltField


type Item
    = Copper
    | Stone
    | Empty


type alias BeltField =
    { pos : Position
    }


createBeltUp : Position -> BeltType
createBeltUp pos =
    BeltUp (Array.fromList [ Copper, Stone ]) { pos = pos }


viewBelt : BeltType -> Element msg
viewBelt beltType =
    el
        [ mouseOver
            [ Background.color (rgb 0.65 0.65 0.65)
            ]
        , Border.width 1
        , Border.solid
        , Border.color (rgb 0.25 0.25 0.25)
        , width <| px 64
        , height <| px 64
        , Background.color (rgb 0.0 1.0 0.0)

        --, Element.Events.onClick <| LeftClickOnField beltType
        ]
        (row []
            [ column []
                [ viewItem
                , viewItem
                , viewItem
                ]
            , column []
                [ viewItem
                , viewItem
                , viewItem
                ]
            ]
        )


viewItem : Element msg
viewItem =
    el
        [ Border.width 1
        , Border.solid
        , Border.color (rgb 0.25 0.25 0.25)
        , moveUp 0
        , width <| px 16
        , height <| px 16
        , Background.color (rgb 0.0 0.55 0.55)
        ]
        (text "")
