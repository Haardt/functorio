module Message exposing (Msg(..))

import Board
import Browser
import Time
import Url


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | BoardMsg Board.Msg
    | Tick Time.Posix
