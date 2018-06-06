module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import List exposing (..)
import List.Extra exposing (..)


type Square
    = X
    | O
    | Empty


type alias Grid =
    List Square


type Player
    = PlayerX
    | PlayerO
    | Cat


type alias Model =
    { grid : Grid
    , player : Player
    }


type Msg
    = Select Int Int


type GameState
    = InProgress
    | Won Player


emptyGrid : Grid
emptyGrid =
    repeat 9 Empty


init : Model
init =
    { grid = emptyGrid
    , player = PlayerX
    }


gameState : Grid -> GameState
gameState grid =
    InProgress


view : Model -> Html Msg
view model =
    svg
        [ viewBox "0 0 300 400"
        , preserveAspectRatio "xMidYMid meet"
        , height "100%"
        , width "100%"
        ]
        [ text_
            [ x "20"
            , y "30"
            , fontFamily "Verdana"
            , fontSize "35"
            ]
            [ text <| toString model.player ]
        , g [ transform "translate(0,50)" ]
            [ viewLines
            , viewGrid model.grid
            ]
        ]


viewGrid : Grid -> Svg Msg
viewGrid grid =
    g []
        (grid
            |> groupsOf 3
            |> List.indexedMap viewRow
            |> List.concat
        )


viewRow : Int -> List Square -> List (Svg Msg)
viewRow rowIndex row =
    row
        |> List.indexedMap (viewSquare rowIndex)


viewSquare : Int -> Int -> Square -> Svg Msg
viewSquare row col square =
    case square of
        Empty ->
            rect
                [ x (toString <| row * 100)
                , y (toString <| col * 100)
                , width "100"
                , height "100"
                , fillOpacity "0"
                , onClick (Select row col)
                ]
                []

        sq ->
            text_
                [ x (toString <| row * 100 + 40)
                , y (toString <| col * 100 + 70)
                , fontFamily "Verdana"
                , fontSize "50"
                ]
                [ text <| toString sq ]


viewLines : Svg Msg
viewLines =
    g []
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
        Select row col ->
            let
                ( nextPlayer, square ) =
                    case model.player of
                        PlayerX ->
                            ( PlayerO, X )

                        _ ->
                            ( PlayerX, O )

                newGrid =
                    setAt (row * 3 + col) square model.grid
            in
                { model | grid = newGrid, player = nextPlayer }


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
