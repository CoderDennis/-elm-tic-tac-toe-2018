module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)


suite : Test
suite =
    describe "Tic-Tac-Toe Tests"
        [ describe "GameState"
            [ test "Game in progress" <|
                \() ->
                    gameState emptyGrid
                        |> Expect.equal InProgress
            ]
        , test "PlayerX won on 2nd row" <|
            \() ->
                gameState
                    [ Empty
                    , O
                    , O
                    , X
                    , X
                    , X
                    , Empty
                    , Empty
                    , Empty
                    ]
                    |> Expect.equal (Won PlayerX)
        , test "PlayerO won on 2nd column" <|
            \() ->
                gameState
                    [ Empty
                    , O
                    , X
                    , X
                    , O
                    , Empty
                    , X
                    , O
                    , Empty
                    ]
                    |> Expect.equal (Won PlayerO)
        ]
