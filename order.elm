module Main exposing (..)

import Html exposing (..)
import Ordering exposing (Ordering)


type alias A =
    { age : Int, svc : Int, id : String }


a : List A
a =
    [ { id = "1", age = 59, svc = 33 }
    , { id = "2", age = 23, svc = 13 }
    , { id = "22", age = 24, svc = 3 }
    , { id = "222", age = 23, svc = 3 }
    , { id = "3", age = 34, svc = 3 }
    ]


aa : List Int
aa =
    [ 3, 1000, -1000, 6, -2, 2, -22, 8, 5, 3 ]


byAgeSvc : Ordering { a | age : comparable, svc : comparable1 }
byAgeSvc =
    Ordering.byField .age
        |> Ordering.breakTiesWith
            (Ordering.byField .svc)


byId : Ordering { a | id : comparable }
byId =
    Ordering.byField .id


byIdAsInt : Ordering { a | id : comparable }
byIdAsInt =
    Ordering.byField .id


ulist : Html msg
ulist =
    ul [] (List.map (\x -> li [] [ text (toString x) ]) (a |> List.sortWith byAgeSvc))


mainz : Html msg
mainz =
    a
        |> List.sortWith byAgeSvc
        |> List.reverse
        |> toString
        |> Html.text


main : Html msg
main =
    ulist
