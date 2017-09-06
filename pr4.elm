module Main exposing (..)

import Html exposing (..)


countElements : List a -> Int
countElements xs =
    List.length xs


main : Html a
main =
    text <|
        case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ toString x ++ " tests."


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
