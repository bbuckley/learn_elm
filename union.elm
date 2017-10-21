module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = SetPbc String


type Ric
    = A
    | B
    | C


enumRic : List Ric
enumRic =
    [ A
    , B
    , C
    ]


stringToMaybeRic : String -> Maybe Ric
stringToMaybeRic string =
    case string of
        "A" ->
            Just A

        "B" ->
            Just B

        "C" ->
            Just C

        _ ->
            Nothing


ricToString : Ric -> String
ricToString ric =
    case ric of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"


list : String
list =
    List.foldl (\a b -> b ++ " " ++ ricToString a) "" enumRic


type alias Model =
    { pbc : Ric
    , mPbc : Maybe Ric
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
            { model | inputPbc = input, mPbc = stringToMaybeRic input }


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text list ]
        , input [ onInput SetPbc, value model.inputPbc ] []
        , p [] [ text (toString model) ]
        ]
