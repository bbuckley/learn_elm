module Main exposing (..)

import Color exposing (black, blue, darkGrey, green, red, white)
import Element exposing (column, el, empty, layout, row, text, wrappedRow)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Set exposing (Set)
import Style
import Style.Border as Border
import Style.Color as Color
import Style.Font exposing (size)


type MyStyles
    = None
    | GreenBorder
    | Pointer


type alias Modelx =
    Int


type alias Model =
    { fil : Int, filterOut : List Int }


stylesheet =
    Style.styleSheet
        [ Style.style None
            [ Border.all 1
            , Border.dotted
            , Color.border black
            ]
        , Style.style Pointer
            [ Style.cursor "pointer"
            ]
        , Style.style GreenBorder
            [ Border.all 1
            , Border.dotted
            , Color.border green
            ]
        ]


sidebarArea model =
    Element.column None
        [ width (percent 20)
        ]
        [ sidebarArea1 model
        , sidebarArea2 model
        ]


sidebarArea1 model =
    Element.column None
        []
        [ Element.text "Sidebar"
        , Element.text "Sidebar"
        , Element.text "Sidebar"
        , Element.text "Sidebar"
        , Element.text "Sidebar"
        ]


r : String -> Element.Element MyStyles variation Msg
r txt =
    row Pointer [ onClick Toggle2 ] [ text txt ]


sidebarArea2 model =
    Element.column None
        []
        [ r ("toggle even (" ++ toString (List.length (List.filter even tcs)) ++ ")")
        , Element.text "toggle 7th"
        , row Pointer [ onClick ShowXs ] [ text "Show Xs" ]
        , Element.text "Sidebar"
        , Element.text "Sidebar"
        ]


contentArea model =
    row None [] [ sidebarArea model, mainContentArea model ]


mainContentArea model =
    wrappedRow None
        [ padding 10, spacing 5, width (percent 80) ]
        (blocks model)


tcs : List Int
tcs =
    List.range 1 145


even : Int -> Bool
even x =
    (%) x 2 == 0


filters : List (Int -> Bool)
filters =
    [ \x -> (<) x 50
    , \x -> (>) x 3
    , \x -> (%) x 7 == 0
    , even
    ]



-- all must be True


filterst : List (Int -> Bool)
filterst =
    [ \x -> (%) x 2 == 0

    -- , \x -> (%) x 3 == 0
    -- , \x -> (%) x 5 == 0
    -- , \x -> (>=) x 40 && (<=) x 100
    ]


filterit : List (Int -> Bool) -> List Int -> List Int
filterit filts list =
    List.foldl (\a b -> List.filter a b) list filts


ftcs model =
    if model.fil == 2 then
        tcs |> filterit filterst
    else if model.fil == 0 then
        Set.toList (Set.intersect (Set.fromList tcs) (Set.fromList model.filterOut))
    else
        filterit filterst tcs



--fftcs =  List.filter (\i -> List.any i filters) tcs
-- |> List.any filters


blocks model =
    List.map (\i -> singleBlock i model) (ftcs model)


chk v model =
    Input.checkbox None
        [ alignRight ]
        { onChange = Check v
        , checked = List.any (\x -> x == v) model.filterOut
        , label = el None [] (text "hello!")
        , options = []
        }


singleBlock value model =
    row None
        [ width (percent 33), height (px 100) ]
        [ column None [ width (percent 50) ] [ text ("TC" ++ toString value) ]
        , column None [ width (percent 50), alignRight ] [ chk value model ]
        ]


singleBlockold value =
    el None
        [ width (percent 33), height (px 100) ]
        (Element.text ("TC" ++ toString value))


pageWrapper model =
    row None
        [ padding 20
        , paddingTop 5
        , paddingBottom 20
        ]
        [ pageArea model ]


pageArea model =
    column None
        [ width (percent 100) ]
        [ headerArea model
        , contentArea model
        , footerArea model
        ]


headerArea model =
    el None
        []
        (text ("Header " ++ toString (List.length (ftcs model)) ++ " of " ++ toString (List.length tcs)))


footerArea model =
    el None
        [ center ]
        (text ("Footer " ++ toString model))


type Msg
    = Toggle2
    | Check Int Bool
    | ShowXs


view : Model -> Html Msg
view model =
    layout stylesheet <| pageWrapper model



-- main : Html msg
-- main =
--     view


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle2 ->
            if model.fil == 2 then
                { model | fil = 3 }
            else
                { model | fil = 2 }

        Check id bool ->
            if bool then
                { model | filterOut = id :: model.filterOut }
            else
                { model | filterOut = List.filter (\x -> id /= x) model.filterOut }

        ShowXs ->
            { model | fil = 0 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { fil = 2, filterOut = [] }
        , update = update
        , view = view
        }
