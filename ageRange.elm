module Main exposing (ageLabel, main, toZero, x, y, zlength, ztrans, ztrans2)

import Html exposing (..)


x : List (Maybe Int)
x =
    [ Nothing, Nothing, Just 20, Just 30, Just 40, Just 30, Nothing ]


y : List (Maybe Int)
y =
    List.filter (\xx -> xx == Just 30) x


toZero : Maybe Int -> Int
toZero i =
    case i of
        Nothing ->
            0

        Just xxx ->
            xxx


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

                Just xxxx ->
                    xxxx
        )
        list


zlength : List (Maybe Int) -> String
zlength list =
    case List.length list of
        0 ->
            "None"

        1 ->
            "It is one"

        ffff ->
            String.fromInt ffff


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
                    String.fromInt (n - 1) ++ " - " ++ String.fromInt n ++ " + "

        -- 2 ->
        --     [ String.fromInt (list.head - 1) ++ " - ", String.fromInt list.tail ++ " + " ]

        other ->
            "other"


main : Html msg
main =
    text (String.fromInt x ++ " " ++ String.fromInt (ztrans x) ++ " " ++ String.fromInt (ztrans2 x))
