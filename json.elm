module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode


type FooBar
    = MyNil
    | Foo
    | Bar


foobarEnum : List FooBar
foobarEnum =
    [ MyNil, Foo, Bar ]



-- fooDecoder : Json.Decode.Decoder FooBar
-- fooDecoder =
--     Json.Decode.map Foo Json.Decode.int
-- foobarDecodel =
--     Json.Decode.oneOf [ fooDecoder ]


main : Program Never Model Msg
main =
    beginnerProgram { model = { duration = 1, fooBar = Foo }, view = view, update = update }


durationOption : Int -> Html Msg
durationOption duration =
    option [ value (toString duration) ] [ text (toString duration) ]


durationOption1 : Model -> List (Html Msg)
durationOption1 model =
    List.map (\x -> option [ value (toString x), selected ((==) x model.duration) ] [ text (toString x) ]) (List.range 1 12)


fooBarOption : Model -> List (Html Msg)
fooBarOption model =
    List.map (\x -> option [ value (toString x) ] [ text (toString x) ]) foobarEnum


fooBarDecoder : Json.Decode.Decoder FooBar
fooBarDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "Foo" ->
                        Json.Decode.succeed Foo

                    "Bar" ->
                        Json.Decode.succeed Bar

                    "MyNil" ->
                        Json.Decode.succeed MyNil

                    somethingElse ->
                        Json.Decode.fail <| "Unknown fooBar: " ++ somethingElse
            )


viewOption : FooBar -> Html Msg
viewOption foobar =
    option
        [ value <| toString foobar ]
        [ text <| toString foobar ]



-- targetValueFooBarDecoder : Json.Decode.Decoder FooBar
-- targetValueFooBarDecoder =
--     targetValue
--         `Json.Decode.andThen`
--             (\val ->
--                 case val of
--                     "Foo" ->
--                         Json.Decode.succeed Foo
--                     "Bar" ->
--                         Json.Decode.succeed Bar
--                     "MyNil" ->
--                         Json.Decode.succeed MyNil
--                     _ ->
--                         Json.Decode.fail ("Invalid Role: " ++ val)
--             )


fooBarDecoderx : Json.Decode.Decoder FooBar
fooBarDecoderx =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "Foo" ->
                        Json.Decode.succeed Foo

                    "Bar" ->
                        Json.Decode.succeed Bar

                    "MyNil" ->
                        Json.Decode.succeed MyNil

                    somethingElse ->
                        Json.Decode.fail <| "Unknown fooBar: " ++ somethingElse
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ h2 [] [ text "Month selector" ]
        , select [ on "change" (Json.Decode.map SetDuration targetValueIntParse) ]
            (List.map durationOption (List.range 1 12))
        , select [ on "change" (Json.Decode.map SetDuration targetValueIntParse) ]
            (List.map (\x -> option [ value (toString x), selected ((==) x model.duration) ] [ text (toString x) ]) (List.range 1 12))
        , select [ on "change" (Json.Decode.map SetDuration targetValueIntParse) ]
            (durationOption1 model)
        , select [ on "change" (Json.Decode.map SetFooBar fooBarDecoder) ]
            (fooBarOption model)
        , div [] [ text <| toString model ]
        ]


type Msg
    = SetDuration Int
    | SetFooBar FooBar


type alias Model =
    { duration : Int, fooBar : FooBar }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetDuration val ->
            -- Debug.log "hererereer"
            { model | duration = val }

        SetFooBar val ->
            { model | fooBar = val }
