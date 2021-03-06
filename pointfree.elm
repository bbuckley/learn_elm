module Main exposing (..)

import Html exposing (..)
import Regex


x : List number
x =
    [ -5, -4 - 3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7 ]


main : Html msg
main =
    -- select 2 x |> toString |> text
    -- lessthan4 x |> toString |> text
    -- x
    --     |> lessthan4
    --     |> isEven
    --     |> List.map (\x -> x + 10)
    --     |> List.map (\x -> x * 10)
    --     |> List.filter (\x -> x % 40 /= 0)
    --     |> List.map (\x -> ( x, True ))
    --     |> toString
    --     |> text
    addCommas "10000000.12345678"
        |> text


select : Int -> List Int -> List Int
select num =
    List.filter (\x -> x < num)


lessthan4 : List number -> List number
lessthan4 =
    List.filter (\x -> x < 4)


isEven : List Int -> List Int
isEven =
    List.filter (\x -> x % 2 == 0)


addCommas : String -> String
addCommas s =
    s
        |> String.reverse
        |> Regex.find Regex.All (Regex.regex "(\\d*\\.)?\\d{0,3}")
        |> List.map .match
        |> String.join ","
        |> String.reverse
