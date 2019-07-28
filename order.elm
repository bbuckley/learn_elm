module Main exposing (main)

import Html exposing (Html, div, li, text, ul)
import Ordering exposing (Ordering, breakTiesWith, byField)


type alias A =
    { age : Int
    , svc : Int
    , id : String
    }


a : List A
a =
    [ { id = "13333", age = 23, svc = 3 }
    , { id = "2", age = 23, svc = 3 }
    , { id = "222", age = 24, svc = 3 }
    , { id = "222", age = 23, svc = 3 }
    , { id = "3", age = 34, svc = 3 }
    , { id = "135", age = 20, svc = 3 }
    ]



-- aa : List Int
-- aa =
--     [ 3, 1000, -1000, 6, -2, 2, -22, 8, 5, 3 ]


byA : Ordering A
byA =
    byField .id
        |> breakTiesWith (byField .svc)
        |> breakTiesWith (byField .age)



-- byId : Ordering { a | id : comparable }
-- byId =
--     Ordering.byField .id
-- byIdAsInt : Ordering { a | id : comparable }
-- byIdAsInt =
--     Ordering.byField .id


ulist : Html msg
ulist =
    ul [] (List.map (\x -> li [] [ text (x.id ++ "/" ++ String.fromInt x.age ++ "/" ++ String.fromInt x.svc) ]) (a |> List.sortWith byA))


ulist1 : Html msg
ulist1 =
    ul [] (List.map (\x -> li [] [ text x ]) [ "a", "aa", "1", "", "2" ])


ulist2 : Html msg
ulist2 =
    ul [] (List.map (\x -> li [] [ text x ]) ([ "a", "aa", "1", "", "2" ] |> List.sortWith Ordering.natural))

stringOrdering : Ordering String
stringOrdering = Ordering.byRank sortBy Ordering.natural 

ulist3 : Html msg
ulist3 =
    ul [] (List.map (\x -> li [] [ text x ]) ([ "3", "a", "aa", "1", "", "2", "", "111" ,""] 
    |> List.sortWith stringOrdering
    --  |> List.sortBy sortBy 
    -- |> Ordering.byRank sortBy Ordering.natural 
    -- |> Ordering.breakTiesWith Ordering.natural
    -- |> List.reverse)
    ))


byS : String -> String -> Order



-- byS = Ordering.reverse Ordering.natural


byS s1 s2 =
    case ( String.toInt s1, String.toInt s2 ) of
        ( _, Nothing ) ->
            LT

        ( Nothing, _ ) ->
            LT

        ( Just i, Just j ) ->
            if j > i then
                GT

            else if j == i then
                EQ

            else
                LT


sortBy : String -> Int
sortBy s =
    case String.toInt s of
        Nothing ->
            2

        Just _ ->
            1



-- mainz : Html msg
-- mainz =
--     a
--         |> List.sortWith byAgeSvc
--         |> List.reverse
--         |> List.map (\x -> String.fromInt x.age)
--         |> String.join "-"
--         |> Html.text


main : Html msg
main =
    div [] [ ulist, ulist1, ulist2, ulist3 ]
