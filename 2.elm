module Main exposing (..)

import Html exposing (..)


x : Int
x =
    2333


main : Html msg
main =
    text ("test " ++ String.fromInt x)
