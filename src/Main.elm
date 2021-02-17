module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Board exposing (Board)
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, el, fill, height, layout, paddingEach, px, rgb, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Field exposing (Field(..))
import GameInit exposing (Game, createGameModel)
import Url
import Position exposing (createPosition, getPositionFromInt)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , game : Game
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        game =
            createGameModel "Unknown"
    in
    ( Model key url game, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Factorio"
    , body =
        [ layout
            [ paddingEach { top = 20, right = 20, left = 20, bottom = 20 }
            , width fill
            , height fill
            ]
          <|
            wrappedRow [ width <| px (64 * 20) ] <|
                viewBoard model.game.board
        ]
    }


viewBoard : Board -> List (Element msg)
viewBoard board =
    let
        length =
            List.range 0 (20 * 12 - 1)
    in
    List.map
        (\pos ->
            getPositionFromInt pos |> Board.getFieldAtPosition board |> box
        )
        length

-- Move to field

box : Field -> Element msg
box field =
    let
        color =
            case field of
                WarehouseFloor ->
                    rgb 1.0 0.0 0

                Belt data ->
                    rgb 0.0 1.0 0

                EmptyField ->
                    rgb 0.3 0.3 0.3
    in
    el [ Border.width 1, Border.solid, Border.color (rgb 0.25 0.25 0.25), width <| px 64, height <| px 64, Background.color color ]
        (text "")
