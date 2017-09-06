import Html exposing (..)
-- import Html.App as App
import Html.Events exposing (..)
import Random
-- import Pr23
-- import Tuple


lotto : Random.Seed -> Int -> Int -> Int -> List Int
lotto seed n low high =
    List.sort [12,3,14,5,6]
      -- <| Tuple.first
      -- <| Pr23.randomSelect seed n [low .. high]
    -- Tuple.first (Pr23.randomSelect seed n [low .. high])


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL


type alias Model =
    { intSeed : Int
    , tested : Bool
    , passed : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1 False False, Cmd.none )



-- UPDATE


type Msg
    = Test
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test ->
            ( model, Random.generate NewFace (Random.int Random.minInt Random.maxInt) )

        NewFace newSeed ->
            ( Model newSeed True True, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Seed value: " ++ (toString (model.intSeed))) ]
        , p [] [ text ("Your lotto numbers are " ++ (toString (lotto (Random.initialSeed model.intSeed) 6 6 49))) ]
        , button [ onClick Test ] [ text "Test" ]
        ]



(..) : Int -> Int -> List Int
(..) start end =
    List.range start end
