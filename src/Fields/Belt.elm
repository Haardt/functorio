module Fields.Belt exposing (BeltField, BeltType(..), Item(..), createBeltUp, viewBelt, updateItemTransport)

import Array exposing (Array)
import Element exposing (Element, centerX, column, el, height, mouseOver, moveUp, padding, px, rgb, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font exposing (center)
import FontAwesome exposing (alignCenter)
import Position exposing (Position, createPosition)


type Item
    = Copper
    | Stone
    | Empty


type Container
    = Left (Array Item)
    | Right (Array Item)
    | Both ( Array Item, Array Item )


type Neighbour
    = HasNeighbour BeltType
    | NoNeighbour


type alias BeltField =
    { pos : Position
    , itemTransportOffset : Int
    , neighbour : Neighbour
    }


type BeltType
    = BeltNorth Container BeltField



--| BeltEast Container BeltField
--| BeltSouth Container BeltField
--| BeltWest Container BeltField


type BeltDirection
    = North
    | East
    | South
    | West



--| BeltDown (Array Item) BeltField
--| BeltLeft (Array Item) BeltField
--| BeltRight (Array Item) BeltField


getNeighbourByDirection : BeltDirection -> Position -> List BeltType -> Neighbour
getNeighbourByDirection direction pos neighbours =
    let
        isNeighbour : Int -> Int -> BeltType -> Bool
        isNeighbour =
            \offsetX offsetY belt ->
                case belt of
                    BeltNorth _ data ->
                        if pos.x + offsetX == data.pos.x && pos.y + offsetY == data.pos.y then
                            True

                        else
                            False

        findNeighbour =
            \x y -> List.filter (isNeighbour x y) neighbours
    in
    Maybe.withDefault NoNeighbour <| Maybe.map HasNeighbour <|
        case direction of
            North ->
                List.head <| findNeighbour 0 -1

            East ->
                List.head <| findNeighbour 1 0

            South ->
                List.head <| findNeighbour -1 0

            West ->
                List.head <| findNeighbour 0 1


createBeltUp : List BeltType -> Position -> BeltType
createBeltUp neighbours pos =
    BeltNorth (Left <| Array.repeat 8 Empty)
        { pos = pos
        , itemTransportOffset = 0
        , neighbour = getNeighbourByDirection North pos neighbours
        }


map : (Container -> BeltField -> Maybe a) -> BeltType -> Maybe a
map fn beltType =
    case beltType of
        BeltNorth container beltField ->
            fn container beltField


updateItemTransport : Int -> BeltType -> BeltType
updateItemTransport tick belt =
    let
        transportOffset =
            modBy 20 (tick // 10)
    in
    if transportOffset == 8 then
        Debug.log "move" belt

    else
        case belt of
            BeltNorth container data ->
                BeltNorth container { data | itemTransportOffset = transportOffset }



--
--updateItemPosition: Int -> Container -> BeltField -> BeltType
--updateItemPosition tick container field =
--    let
--        pos = modBy tick 8
--    in


viewBelt : BeltType -> Element msg
viewBelt beltType =
    Maybe.withDefault (el [] <| text "") (beltType |> map viewItems)


viewItems : Container -> BeltField -> Maybe (Element msg)
viewItems container { pos, itemTransportOffset } =
    Just <|
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
            (row [centerX, spacingXY 4 0]
                [ column [spacingXY 10 6]
                    [ viewItem <| toFloat itemTransportOffset
                    , viewItem <| toFloat itemTransportOffset
                    , viewItem <| toFloat itemTransportOffset
                    ]
                , column [spacingXY 10 6]
                    [ viewItem <| toFloat itemTransportOffset
                    , viewItem <| toFloat itemTransportOffset
                    , viewItem <| toFloat itemTransportOffset
                    ]
                ]
            )


viewItem : Float -> Element msg
viewItem offset =
    el
        [ Border.width 1
        , Border.solid
        , Border.color (rgb 0.25 0.25 0.25)
        , moveUp offset
        , width <| px 16
        , height <| px 16
        , Background.color (rgb 0.0 0.55 0.55)
        ]
        (text "")
