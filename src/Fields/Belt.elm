module Fields.Belt exposing (BeltField, BeltType(..), Item(..), createBeltNorth, updateItemTransport, viewBelt)

import Array exposing (Array)
import Element exposing (Element, centerX, column, el, height, mouseOver, moveUp, px, rgb, row, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Position exposing (Position, createPosition)


type Item
    = Copper Position
    | Stone Position
    | Empty Position


type alias Container =
    ( Array Item, Array Item )


type Neighbour
    = HasNeighbour BeltType
    | NoNeighbour


type alias BeltField =
    { pos : Position
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
    Maybe.withDefault NoNeighbour <|
        Maybe.map HasNeighbour <|
            case direction of
                North ->
                    List.head <| findNeighbour 0 -1

                East ->
                    List.head <| findNeighbour 1 0

                South ->
                    List.head <| findNeighbour -1 0

                West ->
                    List.head <| findNeighbour 0 1


createBeltNorth : List BeltType -> Position -> BeltType
createBeltNorth neighbours pos =
    BeltNorth ( Array.repeat 3 <| Copper <| createPosition 0 0, Array.repeat 3 <| Copper <| createPosition 0 0 )
        { pos = pos
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
    if transportOffset == 19 then
        Debug.log "move" belt

    else
        case belt of
            BeltNorth ( left, right ) data ->
                let
                    newPos : List Item
                    newPos =
                        List.map
                            (\n ->
                                Array.get n left
                                    |> moveContainerItem 0 -1
                            )
                            [ 0, 1, 2 ]
                in
                BeltNorth ( Array.fromList newPos, right ) data


moveContainerItem : Int -> Int -> Maybe Item -> Item
moveContainerItem x y item =
    case item of
        Just (Copper pos) ->
            Copper <| createPosition (pos.x - x) (pos.y - y)

        Just (Stone pos) ->
            Stone <| createPosition (pos.x - x) (pos.y - y)

        Just (Empty pos) ->
            Empty <| createPosition (pos.x - x) (pos.y - y)

        Nothing ->
            Empty <| createPosition 0 0



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
viewItems ( left, right ) { pos } =
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
            ]
            (row [ centerX, spacingXY 4 0 ]
                [ column [ spacingXY 10 6 ]
                    (Array.indexedMap
                        (\index value ->
                            case value of
                                Copper position ->
                                    viewItem <| toFloat position.y

                                Stone position ->
                                    viewItem <| toFloat position.y

                                Empty position ->
                                    viewItem <| toFloat position.y
                        )
                        left
                        |> Array.toList
                    )
                , column [ spacingXY 10 6 ]
                    [ viewItem <| toFloat 0
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
