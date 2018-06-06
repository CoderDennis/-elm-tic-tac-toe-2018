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
        viewLines


viewLines : List (Svg Msg)
viewLines =
    [ viewLine 0 100 300 100
    , viewLine 0 200 300 200
    , viewLine 100 0 100 300
    , viewLine 200 0 200 300
    ]


viewLine : Int -> Int -> Int -> Int -> Svg Msg
viewLine x1_ y1_ x2_ y2_ =
    line
        [ x1 (toString x1_)
        , y1 (toString y1_)
        , x2 (toString x2_)
        , y2 (toString y2_)
        , strokeWidth "5"
        , stroke "black"
        ]
        []


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
