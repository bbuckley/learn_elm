module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = A
    | B
    | C
    | SetPbc String


enumMsg : List Msg
enumMsg =
    [ A
    , B
    , C
    ]


stringToMaybeMsg : String -> Maybe Msg
stringToMaybeMsg string =
    case string of
        "A" ->
            Just A

        "B" ->
            Just B

        "C" ->
            Just C

        _ ->
            Nothing


msgToString : Msg -> String
msgToString msg =
    case msg of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        SetPbc s ->
            "Set"


list : String
list =
    List.foldl (\a b -> b ++ " " ++ msgToString a) "" enumMsg


type alias Model =
    { pbc : Msg
    , mPbc : Maybe Msg
    , inputPbc : String
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = Model A Nothing "newInputPbc"
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPbc input ->
            { model | inputPbc = input, mPbc = stringToMaybeMsg input }

        _ ->
            model


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text list ]
        , input [ onInput SetPbc, value model.inputPbc ] []
        , p [] [ text (toString model) ]
        ]
