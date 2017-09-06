module Main exposing (..)

import Html exposing (..)


x : Int
x =
    2


main : Html msg
main =
    text ("test " ++ toString x)
