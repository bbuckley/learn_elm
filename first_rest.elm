module Main exposing (..)

import Html exposing (div, p, text)
import List


duplicate : List a -> List a
duplicate list =
    case list of
        [] ->
            []

        x :: xs ->
            x :: x :: duplicate xs


range : Int -> List Int -> List String
range first rest =
    case rest of
        [] ->
            [ toString (first - 1) ++ " -", toString first ++ " +" ]

        [ x ] ->
            [ toString (first - 1) ++ " -"
            , toString first ++ " - " ++ toString (x - 1)
            , toString x ++ "+"
            ]

        x :: xx ->
            [ "1", "2", "3" ]


agerange : Int -> List Int -> List Int
agerange first rest =
    let
        x =
            first - 1
    in
    ([ x ] ++ rest) |> List.sort



-- if List.isEmpty rest then
--     [ toString (first - 1) ++ " -", toString first ++ " +" ]
-- else
--     [ "x", "y", "x" ]


main : Html.Html msg
main =
    div []
        [ p []
            [ text (toString (range 20 [])) ]
        , p
            []
            [ text (toString (range 20 [ 24 ])) ]
        , p
            []
            [ text (toString (range 20 [ 24, 29 ])) ]
        ]


test : Bool
test =
    List.all (\( result, expect ) -> result == expect)
        [ ( duplicate [ 1, 2, 3, 5, 8, 8 ], [ 1, 1, 2, 2, 3, 3, 5, 5, 8, 8, 8, 8 ] )
        , ( duplicate [], [] )
        , ( duplicate [ 1 ], [ 1, 1 ] )
        ]
        && List.all (\( result, expect ) -> result == expect)
            [ ( duplicate [ "1", "2", "5" ]
              , [ "1", "1", "2", "2", "5", "5" ]
              )
            ]
