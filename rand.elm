module Main exposing (..)

import Random exposing (Generator)
import Random.Extra


type alias Color =
    String


type alias Model =
    { colors : List Color
    , active : Int
    , real : Maybe Color
    , fake : Maybe Color
    }


initModel : Model
initModel =
    { colors =
        [ "blue"
        , "red"
        , "yellow"
        , "green"
        , "black"
        ]
    , active = 3
    , real = Nothing
    , fake = Nothing
    }


defaultColor =
    "red"


randomColor : Model -> Generator Color
randomColor model =
    Random.Extra.sample (List.take model.active model.colors)
        |> Random.map (Maybe.withDefault defaultColor)


twoRandomColors : Model -> Generator ( Color, Color )
twoRandomColors model =
    Random.pair (randomColor model) (randomColor model)


init : ( Model, Cmd Msg )
init =
    ( initModel, Random.generate InitialColors twoRandomColors )


type Msgg
    = InitialColors ( Color, Color )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialColors ( real, fake ) ->
            ( { model | real = real, fake = fake }, Cmd.none )
