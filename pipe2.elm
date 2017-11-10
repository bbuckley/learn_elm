module Main exposing (..)

import Html exposing (..)


one : One
one =
    1


type alias One =
    Int


test : One -> One
test one =
    one


c : One -> One
c a =
    a |> test |> test


d : One
d =
    test <| test <| test <| (\x -> x * 3) <| (*) 3 <| 2


testString : a -> String -> Int
testString a s =
    String.length s


main : Html msg
main =
    text (toString d)
