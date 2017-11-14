module Main exposing (..)

import Html exposing (..)
import Json.Decode exposing (bool, decodeString, field, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


main =
    xxxxxxx |> toString |> text


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
    decode Point
        |> required "x" int
        |> required "y" int


xxxxx : Result String Point
xxxxx =
    decodeString pointDecoder """{ "x": 3, "y": 4 }"""


type alias Tc =
    { id : String
    , dob : String
    , doe : String
    , crd : String
    , stat : String
    , ric : String
    }


tcDecoder : Json.Decode.Decoder Tc
tcDecoder =
    decode Tc
        |> required "id" string
        |> required "dob" string
        |> required "doe" string
        |> required "crd" string
        |> required "stat" string
        |> required "ric" string


tc1 =
    """{ "id": "123", "dob": "","doe": "","crd": "11","stat": "11","ric": "11" }"""


tc2 =
    """{ "id": "124", "dob": "","doe": "","crd": "11","stat": "11","ric": "11" }"""


tc3 =
    """{ "id": "125", "dob": "","doe": "","crd": "11","stat": "11","ric": "11" }"""


xxxxxx =
    decodeString tcDecoder """{ "id": "123", "dob": "","doe": "","crd": "11","stat": "11","ric": "11" }"""


xxxxxxx =
    decodeString (list tcDecoder) """[{ "id": "123", "dob": "","doe": "","crd": "11","stat": "11","ric": "11" },{ "id": "123", "dob": "","doe": "","crd": "11","stat": "11","ric": "11" }]"""
