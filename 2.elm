module Main exposing (main)

import Html exposing (Html,text)


x : Int
x =
    2333


main : Html msg
main =
    text ("test " ++ String.fromInt x)
