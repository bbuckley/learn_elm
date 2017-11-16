module Main exposing (..)

import Html exposing (..)


x =
    [ -5, -4 - 3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7 ]


main =
    -- select 2 x |> toString |> text
    -- lessthan4 x |> toString |> text
    x |> lessthan4 |> isEven |> toString |> text


select : Int -> List Int -> List Int
select num =
    List.filter (\x -> x < num)


lessthan4 : List number -> List number
lessthan4 =
    List.filter (\x -> x < 4)


isEven : List Int -> List Int
isEven =
    List.filter (\x -> x % 2 == 0)
