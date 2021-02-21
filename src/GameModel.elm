module GameModel exposing (..)


import Board exposing (Board)

type alias Game =
    { board : Board
    , playerName : String
    }
