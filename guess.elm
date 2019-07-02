module Guess exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (title, type_, value)
import Html.Events exposing (onClick, onInput)
import Random



-- Types
--todo use range


type alias Model =
    { typedGuess : Maybe Int -- Nothing if invalid Int
    , submittedGuess : Maybe Int
    , answer : Int
    , totalGuesses : Int
    , lowest : Int
    , highest : Int

    -- , range : ( Int, Int )
    }


type Msg
    = RandomNumberReceived Int
    | TypedText String
    | SubmitGuess



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { typedGuess = Nothing
      , submittedGuess = Nothing
      , answer = 0 -- Default to 0 for now. There are better ways to model this
      , totalGuesses = 0
      , lowest = 0
      , highest = 100

      --   , range = ( 0, 100 )
      }
    , Random.generate RandomNumberReceived (Random.int 1 100)
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomNumberReceived answer ->
            ( { model | answer = answer }
            , Cmd.none
            )

        TypedText inputString ->
            ( { model | typedGuess = String.toInt inputString }
            , Cmd.none
            )

        SubmitGuess ->
            ( { model
                | typedGuess = Nothing
                , submittedGuess = model.typedGuess
                , totalGuesses = model.totalGuesses + 1
                , lowest =
                    max model.lowest (Maybe.withDefault 100 model.typedGuess)
                , highest =
                    min model.highest (Maybe.withDefault 0 model.typedGuess)
              }
            , Cmd.none
            )


theGuess : Model -> Int
theGuess model =
    Maybe.withDefault 0 model.typedGuess


tooHigh : Model -> Bool
tooHigh model =
    theGuess model > model.answer


tooLow : Model -> Bool
tooLow model =
    Maybe.withDefault 0 model.typedGuess < model.answer



-- View


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ input
                [ type_ "text"
                , onInput TypedText
                , model.typedGuess |> Maybe.map String.fromInt |> Maybe.withDefault "" |> value
                ]
                []
            , button
                [ title "submit guess", onClick SubmitGuess ]
                [ text "Guess!" ]
            ]
        , div [] [ text <| "Guesses: " ++ String.fromInt model.totalGuesses ]
        , feedbackText model
        , div [ title "range for next guess" ]
            [ "range = ("
                ++ String.fromInt model.lowest
                ++ ","
                ++ String.fromInt model.highest
                ++ ")"
                |> text
            ]
        ]


feedbackText : Model -> Html Msg
feedbackText model =
    case model.submittedGuess of
        Just guess ->
            if guess == model.answer then
                div [] [ text <| "You correctly guessed " ++ String.fromInt model.answer ]

            else if guess > model.answer then
                div [] [ text "Too high!" ]

            else
                div [] [ text "Too low!" ]

        Nothing ->
            text ""
