--hist.elm


module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import List.Extra
import Pipe1 exposing (personList)


p : List Pipe1.Person
p =
    Pipe1.personList


addN : Int -> Dict Int Int -> Dict Int Int
addN n dict =
    if Dict.member n dict then
        Dict.update n (Maybe.map ((+) 1)) dict
    else
        Dict.insert n 1 dict


freq : List Int -> Dict Int Int
freq list =
    List.foldl addN Dict.empty list


a : List Int
a =
    [ 2, 4, 3, 3, 4, 5, 6, 3, 3 ]


b : List ( Int, Int )
b =
    freq a |> Dict.toList


d : List (List number)
d =
    List.Extra.permutations [ 4, 2, 2, 6 ]


e : List (Maybe Int)
e =
    [ Nothing, Just 3, Just 2, Nothing, Just 2, Just 3, Just 1 ]


f : List (List String)
f =
    List.map
        (\x ->
            case x of
                Nothing ->
                    ""

                Just v ->
                    toString v
        )
        e
        |> List.sort
        |> List.Extra.group


pp : List Pipe1.Person
pp =
    List.filter
        (\x ->
            case x.pbc of
                Nothing ->
                    True

                Just v ->
                    False
        )
        p



-- ids for pbc is Nothing


ids : List String
ids =
    List.map (\x -> x.id) pp



-- ids =
--     List.map (\x -> x.id) pp
-- id = p
--   |


x =
    List.map (\x -> li [] [ text <| toString x ])


main : Html msg
main =
    div []
        [ ul [] (List.map (\x -> li [] [ text (toString x) ]) a)
        , ul [] (List.map (\x -> li [] [ text (toString x) ]) b)
        , ul [] (List.map (\x -> li [] [ text (toString x) ]) ids)
        , ul [] (List.map (\x -> li [] [ text <| toString x ]) ids)
        , ul [] ([ 6, 8, 9 ] |> List.map (\x -> li [] [ text <| toString x ]))
        , ul [] (ids |> x)
        , Html.p [] [ text (toString ids) ]
        ]
