module Main exposing (..)

import Html exposing (..)


x : List Int
x =
    [ 1, 2, 3, 4, 2, 3, 66 ]


main : Html msg
main =
    text ("test " ++ toString x)
