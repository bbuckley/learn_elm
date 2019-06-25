module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    beginnerProgram { model = model, view = view, update = update }


init : { string : String, inc : String }
init =
    { string = "foo", inc = "bar" }


type alias Model =
    { string : String
    , inc : String
    }


model : Model
model =
    init


type StdInc
    = Foo
    | Bar
    | Baz


stdIncs : List StdInc
stdIncs =
    [ Foo, Bar, Baz ]


type Msg
    = SetString String
    | SetInc String
    | AddX
    | DoubleX
    | AddInc
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetString string ->
            { model | string = string }

        SetInc inc ->
            { model | inc = inc }

        AddInc ->
            { model | string = model.string ++ model.inc }

        AddX ->
            { model | string = model.string ++ "x" }

        DoubleX ->
            { model | string = model.string ++ model.string }

        Clear ->
            { model | string = "" }


j : Model -> Html Msg
j model =
    div [] [ input [ onInput SetString, value model.string ] [] ]


view : Model -> Html Msg
view model =
    body []
        [ div [] (List.repeat 2 (j model))
        , p [] [ button [ onClick AddX ] [ text "add x" ] ]
        , p [] [ button [ onClick DoubleX ] [ text "double" ] ]
        , p [] [ button [ onClick Clear ] [ text "clear" ] ]
        , div []
            [ input [ onInput SetInc, value model.inc ] []
            , button [ onClick AddInc ] [ text "add" ]
            ]
        , div []
            [ input [ value model.inc ] []
            ]
        , hr [] []
        , p [] [ text <| toString model ]
        ]
