module Main exposing (..)

import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    Int


initModel : Model
initModel =
    0


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        Reset ->
            0


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [ style [ ( "color", color model ) ] ]
            (List.map viewDot (List.range 1 (abs model)))
        , button [ onClick Increment ] [ text "+" ]
        , p [] [ button [ onClick Reset ] [ text "Reset" ] ]
        ]


color : Int -> String
color x =
    if x < 0 then
        "red"
    else
        "green"


abs : Int -> Int
abs x =
    if x < 0 then
        -x
    else
        x


viewDot : Int -> Html Msg
viewDot n =
    text "*"
