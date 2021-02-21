module Main exposing (Model, init, main, subscriptions, update, view)

import Board exposing (Board)
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignTop, column, fill, height, layout, paddingEach, px, row, text, width, wrappedRow)
import GameInit exposing (createGameModel)
import GameModel exposing (Game)
import Message exposing (Msg(..))
import Url



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

        BoardMsg board ->
            ( { model
                | game =
                    { board = (Tuple.first (Board.updateBoard board model.game.board))
                    , playerName = model.game.playerName
                    }
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Functorio"
    , body =
        [ layout
            [ paddingEach { top = 20, right = 20, left = 20, bottom = 20 }
            , width fill
            , height fill
            ]
          <|
            row [ width fill, height fill ]
                [ column [ width <| px (64 * 20), alignTop ]
                    [ wrappedRow [ width <| px (64 * 20) ] <|
                        (Board.viewBoard model.game.board
                            |> List.map (Element.map BoardMsg)
                        )
                    ]
                , column [ fill |> width, alignTop ]
                    [ text "Hallo"
                    , text "Hallo"
                    ]
                ]
        ]
    }
