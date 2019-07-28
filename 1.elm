module Main exposing (main)

import Html exposing (Html, text)


xxx : Int
xxx =
    11


main : Html msg
main =
    text ("test " ++ String.fromInt xxx)
