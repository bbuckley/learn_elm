module Main exposing (main)

import Html exposing (Html, text)


x : List Int
x =
    [ 1, 2, 3, 4, 2, 3, 66 ]


main : Html msg
main =
    x |> List.map String.fromInt |> String.join "," |> text
