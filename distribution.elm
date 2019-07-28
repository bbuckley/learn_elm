module Main exposing (Hist, Model, Msg(..), addN, addWord, checkbox, config, filtered, freq, freqN, frequency, frequencyN, groupInt, initModel, isChecked, main, sentence, update, view, viewChecked, viewGroupInt, x, y, z)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set


x : List Int
x =
    List.append [ 111, 111 ]
        [ 111, 111, 111, 10, 2, 3, 2, 4, 3, 5, 3, 2 ]


listIntToString : List Int -> String
listIntToString list =
    list |> List.map String.fromInt |> String.concat


listToString : (a -> String) -> List a -> String
listToString a list =
  list |> List.map a |> String.concat
-- listToString a list =
--     list |> List.map a |> String.concat


y : List Int
y =
    List.foldl (::) [] x


z : { a : Int, b : Int }
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
        Just i ->
            List.member i list

        Nothing ->
            False


checkbox : String -> String -> Bool -> Html Msg
checkbox name v bool =
    label []
        [ input [ type_ "checkbox", checked bool, onClick (ToggleInt v) ] []
        , text name
        ]


viewChecked : Model -> List Int -> Html Msg
viewChecked model ele =
    fieldset [] []



-- fieldset []
--     (List.map
--         (\el -> checkbox (listIntToString el) (listIntToString el) (not (List.member el model.out)))
--         ele
--     )
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


foo : Int
foo = 8

groupInt : List { id : Int, desc : String, fn : number -> Bool }
groupInt =
    [ { id = 0, desc = "19 or less", fn = \i -> i < 20 }
    , { id = 1, desc = "20-24", fn = \i -> i >= 20 && i <= 24 }
    , { id = 2, desc = "25-29", fn = \i -> i >= 25 && i <= 29 }
    , { id = 3, desc = "30 or more", fn = \i -> i >= 30 }
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
                Just i ->
                    if not (List.member i model.out) then
                        { model | out = i :: model.out }

                    else
                        { model | out = List.filter (\e -> e /= i) model.out }

                Nothing ->
                    model


config : { first : Int, rest : List Int }
config =
    { first = 20, rest = [ 30, 40, 50, 60 ] }


initModel : Model
initModel =
    Model x [] [ ( 20, True ), ( 30, False ), ( 40, False ), ( 50, False ), ( 60, False ) ] []



-- main : Program Never Model Msg
-- main =
--     Html.beginnerProgram { model = initModel, view = view, update = update }


main : Program () Model Msg
main =
    Browser.element
        { init = (initModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


filtered : Model -> List Int
filtered model =
    -- Set.toList (Set.diff (Set.fromList model.all) (Set.fromList model.out))
    --groupInt (.fn)
    --groupOut [0,1,2,3]
    --model.all
    Set.toList (Set.diff (Set.fromList model.all) (Set.fromList model.out))


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text (listIntToString x ++ " -> " ++ (filtered model |> List.map String.fromInt |> String.concat)) ]

        -- , p [] [ text (toString x ++ " -> " ++ toString frequencyN) ]
        -- , p [] [ text (toString x ++ " -> " ++ toString (freqN x)) ]
        -- , p [] [ text (toString x ++ " -> " ++ toString (Dict.toList (freqN x))) ]
        -- , p [] [ text (toString x ++ " -> " ++ toString (Dict.keys (freqN x))) ]
        , viewChecked model (Dict.keys (freqN x))

        -- , viewAgeCheck model
        , viewGroupInt model
        , p [] [ text "toString model" ]
        ]
