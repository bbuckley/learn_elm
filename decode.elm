module Main exposing (..)

import Html exposing (..)
import Json.Decode exposing (bool, decodeString, field, int, list)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


main =
    xxxxx |> toString |> text


a =
    "[ 1, 2, 3 ]"


y : Result String Bool
y =
    decodeString bool "true"


z : Result String Int
z =
    decodeString int "s27"


zz : String
zz =
    case z of
        Ok x ->
            "it is ok"

        Err e ->
            "the err is " ++ e


ww : Result String (List (List Int))
ww =
    decodeString (list (list int)) "[ [0], [1,2,3], [4,5],[0]   ]"


xxx : Json.Decode.Decoder Int
xxx =
    field "x" int


xxxx : Result String Int
xxxx =
    decodeString xxx """{ "x": 3, "y": 4 }"""


type alias Point =
    { x : Int, y : Int }


pointDecoder : Json.Decode.Decoder Point
pointDecoder =
    decode Point |> required "x" int |> required "y" int


xxxxx : Result String Point
xxxxx =
    decodeString pointDecoder """{ "x": 3, "y": 4 }"""
