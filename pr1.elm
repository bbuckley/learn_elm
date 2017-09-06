module Pr1 exposing (..)

import Html exposing (..)


myLast : List a -> Maybe a
myLast =
    List.reverse >> List.head


main : Html msg
main =
    case myLast [ 1, 2, 3, 4 ] of
        Just a ->
            text (toString a)

        Nothing ->
            text "No element found"
