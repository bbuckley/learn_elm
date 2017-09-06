module Main exposing (..)

import Html exposing (..)
import Pr1 exposing (myLast)


main : Html msg
main =
    case myLast [] of
        Just a ->
            text (toString a)

        Nothing ->
            text "No element found"
