module Main exposing (..)

import Html exposing (..)


x : List (Maybe Int)
x =
    [ Nothing, Nothing, Just 20, Just 30, Just 40, Just 30, Nothing ]


y : List (Maybe Int)
y =
    List.filter (\x -> x == Just 30) x


toZero : Maybe Int -> Int
toZero i =
    case i of
        Nothing ->
            0

        Just x ->
            x


ztrans : List (Maybe Int) -> List Int
ztrans list =
    List.map toZero list


ztrans2 : List (Maybe Int) -> List Int
ztrans2 list =
    List.map
        (\i ->
            case i of
                Nothing ->
                    0

                Just x ->
                    x
        )
        list


zlength : List (Maybe Int) -> String
zlength list =
    case List.length list of
        0 ->
            "None"

        1 ->
            "It is one"

        x ->
            toString x


ageLabel : List Int -> String
ageLabel list =
    case List.length list of
        0 ->
            ""

        1 ->
            case List.head list of
                Nothing ->
                    "nothiong"

                Just n ->
                    toString (n - 1) ++ " - " ++ toString n ++ " + "

        -- 2 ->
        --     [ toString (list.head - 1) ++ " - ", toString list.tail ++ " + " ]
        x ->
            toString x


main : Html msg
main =
    text (toString x ++ " " ++ toString (ztrans x) ++ " " ++ toString (ztrans2 x))
