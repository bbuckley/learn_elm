module Pr5 exposing (..)

import Html exposing (..)


myReverse : List a -> List a
myReverse =
    List.foldl (::) []



-- case x of
--     [] ->
--         []
--
--     head :: tail ->
--         myReverse tail ++ [ head ]


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
            [ myReverse [ 1, 2 ] == [ 2, 1 ]
            , myReverse [] == []
            , myReverse [ 1, 2, 3 ] == [ 3, 2, 1 ]
            ]
