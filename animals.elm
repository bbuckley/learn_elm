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
        [ select [ onInput x ] (List.map (\x -> viewOptions2 model x) model.animals)
        , select [ onInput x ] (List.map (\x -> viewOptions2 model x) model.animals)
        , Maybe.map .name model.selected
            |> Maybe.withDefault "--"
            |> text
        , p [] [ text (toString x) ]
        ]


xx : Animal -> Html.Attribute msg
xx animal =
    value <| toString animal.id


sel : Model -> Animal -> Bool
sel model animal =
    case model.selected of
        Nothing ->
            False

        Just v ->
            animal.id == v.id



-- msel : Model -> Maybe Animal -> Bool
-- msel model manimal =
--     case manimal of
--         Nothing ->
--             model.selected == Nothing
--         Just v ->
--             model.selected == v
-- viewOptions : Animal -> Html msg
-- viewOptions animal =
--     option [ value <| toString animal.id ] [ text animal.name ]


viewOptions2 : Model -> Animal -> Html msg
viewOptions2 model animal =
    option [ xx animal, selected <| sel model animal ] [ text animal.name ]


xxx : Maybe Animal -> Html.Attribute msg
xxx animal =
    case animal of
        Nothing ->
            value <| "---"

        Just v ->
            value <| toString v.id



-- viewOptions3 : Model -> Maybe Animal -> Html msg
-- viewOptions3 model animal =
--     option [ xxx animal, selected <| msel model animal ] [ text animal.name ]


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
