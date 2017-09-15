module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set


x : List Int
x =
    List.append [ 111, 111 ]
        [ 111, 111, 111, 10, 2, 3, 2, 4, 3, 5, 3, 2 ]


y =
    List.foldl (::) [] x


z =
    { a = 888, b = 4 }


sentence : String
sentence =
    "with this is a sentence with with with a few words in it"


addWord : String -> Dict String Int -> Dict String Int
addWord word dict =
    if Dict.member word dict then
        Dict.update word (Maybe.map ((+) 1)) dict
    else
        Dict.insert word 1 dict


frequency : Dict String Int
frequency =
    List.foldl addWord Dict.empty (String.split " " sentence)


type alias Hist =
    Dict Int Int


addN : Int -> Hist -> Hist
addN n dict =
    if Dict.member n dict then
        Dict.update n (Maybe.map ((+) 1)) dict
    else
        Dict.insert n 1 dict


frequencyN : Dict Int Int
frequencyN =
    List.foldl addN Dict.empty x


freqN : List Int -> Dict Int Int
freqN list =
    List.foldl addN Dict.empty list


freq : List Int -> Dict Int Int
freq list =
    List.foldl addN Dict.empty list


isChecked : List Int -> String -> Bool
isChecked list str =
    case String.toInt str of
        Ok i ->
            List.member i list

        Err msg ->
            False


checkbox : String -> String -> Bool -> Html Msg
checkbox name v bool =
    label []
        [ input [ type_ "checkbox", checked bool, onClick (ToggleInt v) ] []
        , text name
        ]


viewChecked : Model -> List Int -> Html Msg
viewChecked model ele =
    fieldset []
        (List.map
            (\el -> checkbox (toString el) (toString el) (not (List.member el model.out)))
            ele
        )



-- ageRange: List Int -> String
-- ageRange =
--   case []-> "Invalid"
-- viewAgeCheck : Model -> Html Msg
-- viewAgeCheck model =
--     fieldset []
--         ([ checkbox (toString (config.first - 1) ++ "-") "s" True
--          ]
--             ++ List.map
--                 (\el ->
--                     (label []
--                         [ input [ type_ "checkbox", checked bool, onClick (ToggleInt v) ] []
--                         , text name
--                         ]
--                 )
--                 config.rest
--         )
--


viewGroupInt : Model -> Html Msg
viewGroupInt model =
    fieldset []
        (List.map
            (\el ->
                label []
                    [ input [ type_ "checkbox", checked True, onClick (ToggleGroupInt el.id) ] []
                    , text el.desc
                    ]
            )
            groupInt
        )


type Msg
    = ToggleInt String
    | ToggleGroupInt Int


type alias Model =
    { all : List Int
    , out : List Int
    , rangeOut : List ( Int, Bool )
    , groupOut : List Int
    }


groupInt : List { id : Int, desc : String, fn : number -> Bool }
groupInt =
    [ { id = 0, desc = "19 or less", fn = \x -> x < 20 }
    , { id = 1, desc = "20-24", fn = \x -> x >= 20 && x <= 24 }
    , { id = 2, desc = "25-29", fn = \x -> x >= 25 && x <= 29 }
    , { id = 3, desc = "30 or more", fn = \x -> x >= 30 }
    ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleGroupInt i ->
            -- { model | groupOut = id :: model.groupOut }
            if not (List.member i model.groupOut) then
                { model | groupOut = i :: model.groupOut }
            else
                { model | groupOut = List.filter (\e -> e /= i) model.groupOut }

        ToggleInt str ->
            case String.toInt str of
                Ok i ->
                    if not (List.member i model.out) then
                        { model | out = i :: model.out }
                    else
                        { model | out = List.filter (\e -> e /= i) model.out }

                Err msg ->
                    model


config : { first : Int, rest : List Int }
config =
    { first = 20, rest = [ 30, 40, 50, 60 ] }


initModel : Model
initModel =
    Model x [] [ ( 20, True ), ( 30, False ), ( 40, False ), ( 50, False ), ( 60, False ) ] []


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


filtered : Model -> List Int
filtered model =
    Set.toList (Set.diff (Set.fromList model.all) (Set.fromList model.out))


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text (toString x ++ " -> " ++ toString (filtered model)) ]

        -- , p [] [ text (toString x ++ " -> " ++ toString frequencyN) ]
        -- , p [] [ text (toString x ++ " -> " ++ toString (freqN x)) ]
        -- , p [] [ text (toString x ++ " -> " ++ toString (Dict.toList (freqN x))) ]
        -- , p [] [ text (toString x ++ " -> " ++ toString (Dict.keys (freqN x))) ]
        , viewChecked model (Dict.keys (freqN x))

        -- , viewAgeCheck model
        , viewGroupInt model
        , p [] [ text (toString model) ]
        ]
