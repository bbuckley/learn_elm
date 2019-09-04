module Main exposing (main)

import Html exposing (Html, text)


x : List a -> Int
x =
    List.foldl (\_ a -> 1 + a) 0


main : Html msg
main =
    "tester " ++ String.fromInt (x [ 1, 7, 8 ]) |> text
