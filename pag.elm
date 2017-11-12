module Main exposing (..)

import Html exposing (..)
import Paginate exposing (PaginatedList, fromList, goTo, page)


xx =
    List.range 1 100


main =
    text (toString z)


x : List Int
x =
    -- equals [ 21, 22, 23, 24, 25, 26, 27, 28, 29 30 ]
    xx |> fromList 10 |> goTo 3 |> page


y : List Int
y =
    page <| goTo 3 <| fromList 10 <| xx


z =
    fromList 10 <| xx
