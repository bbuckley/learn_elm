module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


countElements : List a -> Int
countElements xs =
    List.length xs


main : Html a
main =
    let
        x =
            case test of
                0 ->
                    "Your implementation passed all tests."

                1 ->
                    "Your implementation failed one test."

                x ->
                    "Your implementation failed " ++ toString x ++ " tests."
    in
    h3 [ style [ ( "color", color test ) ] ] [ text x ]


color : Int -> String
color x =
    if x == 0 then
        "green"
    else
        "red"


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ countElements (List.range 1 4000) == 4000
            , countElements [ 1 ] == 1
            , countElements [] == 0
            , countElements [ 'a', 'b', 'c' ] == 3
            , countElements [ 'a', 'b', 'c', 'd' ] == 4
            ]
