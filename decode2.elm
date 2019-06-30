module Decode2 exposing (User, anonDecoder, manDecoder, personDecoder, result, userDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)


type Sex
    = Male
    | Female


type Client
    = Anon
    | Person Ben
    | Man Int


anonDecoder : Decoder Client
anonDecoder =
    Decode.succeed Anon


personDecoder : Decoder Client
personDecoder =
    Decode.map Person benDecoder


manDecoder : Decoder Client
manDecoder =
    Decode.map Man (Decode.field "i" Decode.int)


clientDecoder : Decode.Decoder Client
clientDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Anon" ->
                        anonDecoder

                    "Person" ->
                        personDecoder

                    "Man" ->
                        manDecoder

                    _ ->
                        Decode.fail ("Invalid Client: " ++ string)
            )


sexDecoder : Decode.Decoder Sex
sexDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "Male" ->
                        Decode.succeed Male

                    "Female" ->
                        Decode.succeed Female

                    _ ->
                        Decode.fail ("Invalid Spouse: " ++ string)
            )


type alias Ben =
    { id : Int
    , name : String
    , sex : Sex
    }


type alias User =
    { id : Int
    , name : String
    , email : String
    , sex : Sex
    , sexes : List Sex
    , ben : Ben
    , bens : List Ben
    , clients : List Client
    }


benDecoder : Decoder Ben
benDecoder =
    Decode.succeed Ben
        |> required "id" int
        |> required "name" string
        |> required "sex" sexDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" int
        |> required "name" string
        |> required "email" string
        |> required "sex" sexDecoder
        |> required "sexes" (Decode.list sexDecoder)
        |> required "ben" benDecoder
        |> required "bens" (Decode.list benDecoder)
        |> required "clients" (Decode.list clientDecoder)



result : Result Decode.Error User
result =
    Decode.decodeString userDecoder 
 """
 {
"id": 123, 
"email": "sam@example.com", "name": "Sam", 
"sex": "Male",
"sexes": ["Male","Female","Female"],
"ben": {"id":45, "name":"","sex": "Male"},
"bens": [{"id":45, "name":"","sex":"Female"},{"id":46, "name":"Joe","sex":"Female"}],
"clients": [
    {"type":"Anon"},
    {"type":"Person", "sex": "Male","name":"Brian","id": 77},
    {"type":"Person", "sex": "Male","name":"B","id": 79},
    {"type":"Man", "sex": "Female", "i": 79}
    ]
 }
 """
