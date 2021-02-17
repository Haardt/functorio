module Field exposing (Direction(..), Field(..), FieldId(..), WarehouseFloor(..))


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
    }


type Field
    = EmptyField
    | WarehouseFloor
    | Belt BeltField
