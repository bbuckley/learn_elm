module Main exposing (..)

import Html exposing (..)


xxx : Int
xxx =
    1


main : Html msg
main =
    text ("test " ++ toString xxx)
