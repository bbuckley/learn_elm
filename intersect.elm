module Main exposing (..)

import Color exposing (black, blue, darkGrey, green, red, white)
import Element exposing (column, el, empty, row, text)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Set exposing (Set)
import Style
import Style.Border as Border
import Style.Color as Color
import Style.Font exposing (size)


type MyStyles
    = Title
    | Header
    | Foo
    | None
    | NavOption
    | UserMenu
    | DropDownOptions


stylesheet : Style.StyleSheet MyStyles variation
stylesheet =
    Style.styleSheet
        [ Style.style Header
            [ size 25 ]
        , Style.style None
            []
        , Style.style NavOption
            [ Border.all 1
            , Border.dotted
            , Color.border black
            ]
        , Style.style UserMenu
            []
        , Style.style DropDownOptions
            []
        , Style.style Title
            [ Color.text blue
            , Color.background white
            , size 25
            , Border.all 1
            , Border.solid
            , Color.border Color.green
            ]
        , Style.style Foo
            [ Color.text green
            , Color.background white
            , size 30
            , Border.all 1
            , Border.solid
            , Color.border red
            ]
        ]


ll : List (List Int)
ll =
    [ [ 11, 1, 3, 2 ], [ 11, 2, 3, 4, 1 ], [ 11, 2, 22, 33, 1, 7 ] ]


lll : List { dob : String, dot : String, id : String }
lll =
    [ { id = "1", dob = "", dot = "" }, { id = "2", dob = "", dot = "" }, { id = "3", dob = "", dot = "" } ]


cc =
    column NavOption
        [ padding 10, spacing 5, height fill ]
        ([ el Header [] (text "xxxx")
         , xx "1"
         , xx "1"
         ]
            ++ List.map (\x -> xx x.id) lll
        )


xx x =
    row Title
        [ padding 10, spacing 5 ]
        [ el None [] (text "Hello")
        , el Foo [] (text x)
        , xxxx
        , xxx
        ]


blah =
    el None [] (text "blah")


xxx =
    column Title
        [ padding 10, spacing 10 ]
        [ el None [] (text "Hello")
        , el Foo [] (text "World")
        , blah
        , el Title [] (text "!")
        , el Title [] (text "!")
        ]


xxxx =
    row Foo
        [ spacing 15, padding 15, alignRight, width fill ]
        [ el None [] empty
        , el None [] empty
        , el None [] empty
        ]


e : Element.Element MyStyles variation msg
e =
    el Title [] (text "hello!")


view : Html msg
view =
    Element.layout stylesheet <|
        cc


intersectAll5 : List (List comparable) -> List comparable
intersectAll5 =
    List.concat >> Set.fromList >> Set.toList


intersectAll4 : List (List comparable) -> List comparable
intersectAll4 l =
    List.concat l |> Set.fromList |> Set.toList


intersectAll3 : List (List comparable) -> List comparable
intersectAll3 lists =
    Set.fromList (List.concat lists) |> Set.toList


intersectAll2 : List (List comparable) -> List comparable
intersectAll2 lists =
    case lists of
        [] ->
            []

        first :: rest ->
            Set.fromList (List.concat (first :: rest)) |> Set.toList


intersectAll : List (List comparable) -> List comparable
intersectAll lists =
    case lists of
        [] ->
            []

        first :: rest ->
            let
                fromList : List comparable -> Set comparable
                fromList list =
                    Set.fromList list
            in
            List.foldl
                (\list acc -> Set.intersect (fromList list) acc)
                (fromList first)
                rest
                |> Set.toList


main : Html msg
main =
    view



--ll |> intersectAll |> toString |> text
