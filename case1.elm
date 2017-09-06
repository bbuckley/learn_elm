module Main exposing (..)

import Html exposing (..)


n : Int
n =
    120


main : Html a
main =
    text <|
        case n of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            4 ->
                toString n

            (-1) ->
                toString n

            x ->
                "x was caught on n = " ++ toString x
