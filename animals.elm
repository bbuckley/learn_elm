module Main exposing (main)

import Html exposing (Html, div, option, p, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, update = update, view = view }


type alias Model =
    { animals : List Animal
    , selected : Maybe Animal
    }


type alias Animal =
    { id : String
    , name : String
    , age : Maybe Int
    }


type Msg
    = SelectMessage (Maybe Animal)


model : Model
model =
    { animals = [ Animal "0" "Bär" (Just 34), Animal "1" "Dolphin" (Just 33), Animal "3" "Bee" (Just 2), Animal "4" "Ant" Nothing ]
    , selected = Nothing
    }


x : String -> Msg
x =
    SelectMessage << getAnimalFromId model.animals


view : Model -> Html Msg
view model =
    div []
        [ select
            [ onInput x ]
            (List.map viewOptions model.animals)
        , select [ onInput x ]
            (List.map viewOptions model.animals)
        , select [ onInput x ]
            (List.map viewOptions model.animals)
        , select [ onInput x ]
            (List.map viewOptions model.animals)
        , Maybe.map .name model.selected
            |> Maybe.withDefault "--"
            |> text
        , p [] [ text (toString x) ]
        ]


xx : Animal -> Html.Attribute msg
xx animal =
    value <| toString animal.id


viewOptions : Animal -> Html msg
viewOptions animal =
    --  option [ value <| toString animal.id ] [ text animal.name ]
    option [ xx animal, selected <| False ] [ text animal.name ]


getAnimalFromId : List Animal -> String -> Maybe Animal
getAnimalFromId animals id =
    List.filter (\a -> toString a.id == id) animals |> List.head


getIdFromAnimal : List Animal -> Maybe Animal -> Maybe String
getIdFromAnimal animals maybeAnimal =
    case maybeAnimal of
        Nothing ->
            Nothing

        Just v ->
            let
                animal =
                    List.filter (\a -> toString a.id == v.id) animals |> List.head
            in
            case animal of
                Nothing ->
                    Nothing

                Just v ->
                    Just v.id



--List.filter (\a -> a.id == id) animals |> List.head


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectMessage mbAnimal ->
            { model | selected = mbAnimal }
