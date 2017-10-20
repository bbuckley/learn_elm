module Main exposing (..)

import Html exposing (..)


xxx : Int
xxx =
    11


main : Html msg
main =
    text ("test " ++ toString xxx)
