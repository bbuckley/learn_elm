module Main exposing (main)

import Html exposing (Html, text)


xxx : Int
xxx =
    -- 11
    --List.foldl (\_ a -> 1 + a) 0 (List.repeat 3 11)
    List.repeat 4 "" |> List.foldl (\_ a -> 1 + a) 0


main : Html msg
main =
    -- text ("test " ++ String.fromInt xxx)
    "tester " ++ String.fromInt xxx |> text
