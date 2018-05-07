-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html


module Main exposing (..)

import Html exposing (Html, a, beginnerProgram, button, div, input, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


model =
    { count = 0, inc = 1, incString = "1", incError = "", view = MainView }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | Set100
    | Add5
    | SetInc String
    | ToggleView View
    | SetView View


update msg model =
    case msg of
        Increment ->
            { model | count = (+) model.count model.inc }

        Decrement ->
            { model | count = (-) model.count model.inc }

        Reset ->
            { model | count = 0, inc = 1, incString = "1" }

        Set100 ->
            { model | count = 100 }

        Add5 ->
            { model | count = (+) model.count 5 }

        SetInc string ->
            case String.toInt string of
                Ok v ->
                    { model | incString = string, inc = v, incError = "" }

                Err e ->
                    { model | incString = string, incError = e }

        ToggleView v ->
            { model | view = v }

        SetView v ->
            { model | view = v }



-- if model.view == MainView then
--     { model | view = ConfigView }
-- else
--     { model | view = MainView }


b : Msg -> String -> Html Msg
b msg txt =
    button [ onClick msg ] [ text txt ]


m : List View
m =
    [ MainView, ConfigView, EditView ]


x model =
    div []
        (List.map
            (\x -> span [] [ a [ style [ ( "fontSize", "14" ) ], onClick (SetView x) ] [ text (toString x) ] ])
            m
        )


type Sign
    = Pos
    | Neg


type View
    = MainView
    | ConfigView
    | EditView


label sign model =
    case sign of
        Pos ->
            "+" ++ toString model.inc

        Neg ->
            "-" ++ toString model.inc



-- VIEW


view1 model =
    div []
        [ x model
        , button [ onClick Decrement ] [ text ("-" ++ toString model.inc) ]
        , div [] [ text (toString model.count ++ " " ++ String.repeat (abs model.count) "*") ]
        , button [ onClick Increment ] [ text ("+" ++ toString model.inc) ]
        , b Set100 "set100"
        , b Decrement (label Neg model)
        , b Increment (label Pos model)
        , b Add5 "add 5"
        , b Reset "reset"
        , input [ onInput SetInc, value model.incString ] []
        , p [] [ text (toString model) ]
        ]


view model =
    case model.view of
        MainView ->
            view1 model

        ConfigView ->
            div [] [ x model, p [] [ text (toString model) ] ]

        EditView ->
            div [] [ x model, p [] [ text (toString model) ] ]
