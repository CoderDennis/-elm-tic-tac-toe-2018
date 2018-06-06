module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    String


type Msg
    = NoOp


init : Model
init =
    "Hello Worksoft!"


view : Model -> Html Msg
view model =
    svg
        [ viewBox "0 0 300 400"
        , preserveAspectRatio "xMidYMid meet"
        , height "100%"
        , width "100%"
        ]
        [ text_
            [ x "0"
            , y "35"
            , fontFamily "Verdana"
            , fontSize "35"
            ]
            [ text model ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
