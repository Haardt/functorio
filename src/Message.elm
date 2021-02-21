module Message exposing (..)

import Board
import Browser
import Url

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | BoardMsg Board.Msg
