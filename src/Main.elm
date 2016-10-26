port module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { audio : String
    , playing : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { audio = "static/media/buccaneers.mp3"
      , playing = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Play
    | Pause


port play : () -> Cmd msg


port pause : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( { model | playing = True }, play () )

        Pause ->
            ( { model | playing = False }, pause () )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "audio" ]
        [ audio
            [ id "audiofile"
            , src model.audio
            , controls True
            ]
            []
        , viewPlayButton model.playing
        ]


viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    if playing then
        button
            [ class "pause"
            , name "pause"
            , onClick Pause
            ]
            [ text "Pause" ]
    else
        button
            [ class "play"
            , name "play"
            , onClick Play
            ]
            [ text "Play" ]



-- MAIN


main : Program Never
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
