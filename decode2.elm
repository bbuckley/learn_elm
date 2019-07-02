module Decode2 exposing (Ben, Client(..), Sex(..), User, anonDecoder, benDecoder, benEncoder, clientDecoder, manDecoder, personDecoder, result, sexDecoder, sexEncoder, userDecoder, userEncoder)

-- import Json.Encode.Pipeline exposing (list)

import Html exposing (..)
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



--exposing (list)
--exposing (Value, list)


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


json_before =
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
    {"type":"Man", "i": 79, "foo" : "ioioioioioioioi"}
    ]
 }
 """


result : String -> Result Decode.Error User
result s =
    Decode.decodeString userDecoder s


benEncoder : Ben -> Encode.Value
benEncoder record =
    Encode.object
        [ ( "id", Encode.int <| record.id )
        , ( "name", Encode.string <| record.name )
        , ( "sex", sexEncoder <| record.sex )
        ]


sexEncoder : Sex -> Encode.Value
sexEncoder sex =
    case sex of
        Male ->
            Encode.string "Male"

        Female ->
            Encode.string "Female"


clientEncoder : Client -> Encode.Value
clientEncoder client =
    case client of
        Anon ->
            Encode.object
                [ ( "type", Encode.string <| "Anon" ) ]

        Person { id, name, sex } ->
            Encode.object
                [ ( "type", Encode.string "Person" )
                , ( "id", Encode.int id )
                , ( "name", Encode.string name )
                , ( "sex", sexEncoder sex )
                ]

        -- Person ben ->
        --     benEncoder ben
        Man i ->
            Encode.object
                [ ( "type", Encode.string <| "Man" )
                , ( "i", Encode.int <| i )
                ]


userEncoder : User -> Encode.Value
userEncoder record =
    Encode.object
        [ ( "id", Encode.int <| record.id )
        , ( "email", Encode.string <| record.email )
        , ( "name", Encode.string <| record.name )
        , ( "sex", sexEncoder <| record.sex )
        , ( "sexes", Encode.list identity <| List.map sexEncoder <| record.sexes )
        , ( "ben", record.ben |> benEncoder )
        , ( "bens", Encode.list identity <| List.map benEncoder <| record.bens )
        , ( "clients", Encode.list identity <| List.map clientEncoder <| record.clients )
        ]


json_after : String -> String
json_after before =
    case Decode.decodeString userDecoder before of
        Ok user ->
            user |> userEncoder |> Encode.encode 2

        Err e ->
            "not good"


main : Html msg
main =
    div []
        [ div [] [ text json_before ]
        , hr [] []
        , div [] [ text (json_before |> json_after) ]
        , hr [] []
        , div [] [ text (json_before |> json_after |> json_after) ]
        ]
