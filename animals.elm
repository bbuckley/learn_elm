module Main exposing (main)

import Html exposing (Html, div, option, p, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


main =
    Html.beginnerProgram { model = model, update = update, view = view }


type alias Model =
    { animals : List Animal
    , selected : Maybe Animal
    }


type alias Animal =
    { id : String
    , name : String
    }


type Msg
    = SelectMessage (Maybe Animal)


model : Model
model =
    { animals = [ Animal "0" "Bär", Animal "1" "Dolphin", Animal "3" "Bee", Animal "4" "Ant" ]
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
            --    [ onInput (SelectMessage << getAnimalFromId model.animals) ]
            (List.map viewOptions model.animals)
        , Maybe.map .name model.selected
            |> Maybe.withDefault "--"
            |> text
        , p [] [ text (toString model) ]
        , p [] [ text (toString x) ]
        ]


xx : Animal -> Html.Attribute msg
xx animal =
    value <| toString animal.id


viewOptions : Animal -> Html msg
viewOptions animal =
    --  option [ value <| toString animal.id ] [ text animal.name ]
    option [ xx animal ] [ text animal.name ]


getAnimalFromId : List Animal -> String -> Maybe Animal
getAnimalFromId animals id =
    -- List.filter (\a -> toString a.id == id) animals |> List.head
    List.filter (\a -> a.id == id) animals |> List.head


update msg model =
    case msg of
        SelectMessage mbAnimal ->
            { model | selected = mbAnimal }
