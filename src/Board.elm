module Board exposing (Board, Msg(..), ViewPort, updateBoard, viewBoard)

import Dict exposing (Dict)
import Element exposing (Element)
import Field exposing (Field(..), FieldId, Msg(..), createFieldId, getFieldId)
import Fields.Belt exposing (BeltType(..), Item(..), createBeltNorth)
import Fields.Neighbour as Neighbour
import Fields.Belt as Belt
import Position exposing (Position, createPosition, getPositionFromInt)


type Msg
    = FieldMsg Field.Msg
    | Tick


type alias Zoom =
    Int


type alias ViewPort =
    { pos : Position
    , z : Zoom
    }


type alias Board =
    { fields : Dict FieldId Field
    , viewPort : ViewPort
    }


updateBoard : Msg -> Int -> Board -> ( Board, Cmd Msg )
updateBoard msg time model =
    case msg of
        FieldMsg field ->
            case field of
                LeftClickOnField fieldType ->
                    case fieldType of
                        WarehouseFloor pos ->
                            let
                                id =
                                    createFieldId pos.x pos.y

                                getFieldById =
                                    \fieldId -> Dict.get fieldId model.fields

                                neighbours =
                                    Neighbour.getFieldIds pos
                                        |> List.map getFieldById
                                        |> List.filterMap identity
                                        |> List.filter Field.isBelt
                                        |> List.map getBeltType
                                        |> List.filterMap identity

                                newModel =
                                    { model
                                        | fields =
                                            Dict.insert id
                                                (Belt (createBeltNorth neighbours pos))
                                                model.fields
                                    }
                            in
                            ( newModel, Cmd.none )

                        Belt _ ->
                            ( model, Cmd.none )

        Tick ->
            let
                belts =
                    Dict.values model.fields
                        |> List.filter Field.isBelt
                        |> List.map getBeltType
                        |> List.filterMap identity

                updatedBelts =
                    List.map (transportItems belts) belts
                        |> List.map (\n -> ( getFieldId n, Belt <| Belt.updateItemTransport time n ))
                        |> Dict.fromList
            in
            ( { model
                | fields =
                    Dict.union updatedBelts model.fields
              }
            , Cmd.none
            )


transportItems : List BeltType -> BeltType -> BeltType
transportItems beltTypes beltType =
    case beltType of
        BeltNorth item beltField ->
            BeltNorth item { beltField | pos = createPosition beltField.pos.x beltField.pos.y }


getBeltType : Field -> Maybe BeltType
getBeltType field =
    case field of
        Belt (BeltNorth item data) ->
            BeltNorth item data |> Just

        _ ->
            Nothing


viewBoard : Board -> Int -> List (Element Msg) -> List (Element Msg)
viewBoard board pos resultList =
    if pos == -1 then
        resultList

    else
        viewBoard board
            (pos - 1)
            (resultList
                ++ [ getPositionFromInt pos
                        |> getFieldFromId board
                        |> Field.viewField
                        |> Element.map FieldMsg
                   ]
            )


getFieldFromId : Board -> Position -> Field
getFieldFromId board pos =
    Dict.get (createFieldId pos.x pos.y) board.fields
        |> Maybe.withDefault (WarehouseFloor pos)
