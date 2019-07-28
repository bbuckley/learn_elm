module Main exposing (main)

import Html exposing (text,  Html)


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
                String.fromInt n

            -- -(1)  ->
            --      String.fromInt n

            x ->
                "x was caught on n = " ++ String.fromInt x
