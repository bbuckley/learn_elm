module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = SetRic String


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
    { ric : Ric
    , mRic : Maybe Ric
    , inputRic : String
    }


initialModel : Model
initialModel =
    { ric = A
    , mRic = Just A
    , inputRic = "A"
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetRic input ->
            { model
                | inputRic = input
                , mRic = stringToMaybeRic input
                , ric = Maybe.withDefault model.ric (stringToMaybeRic input)
            }


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text list ]
        , input [ placeholder "enter ric", onInput SetRic, value model.inputRic ] []
        , p [] [ text (toString model) ]
        ]
