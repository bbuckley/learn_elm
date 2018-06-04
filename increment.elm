module Main exposing (..)

--import Style.Color as Color

import Color exposing (black, blue, darkGrey, green, red, white)
import Element exposing (Element, column, el, empty, layout, row, text, wrappedRow)
import Element.Attributes exposing (padding, paddingTop, spacing)
import Element.Events exposing (onClick, onInput)
import Element.Input exposing (..)
import Html exposing (Html, a, beginnerProgram, div, input, p, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Style
import Style.Border as Border
import Style.Color as Color


type MyStyles
    = None
    | GreenBorder
    | Pointer
    | Spaced
    | Button


stylesheet : Style.StyleSheet MyStyles variation
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
        , Style.style Spaced
            []
        , Style.style Button
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.blue
            , Color.background Color.lightBlue
            ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { count : Int
    , inc : Int
    , incString : String
    , incError : String
    , view : View
    }


model : Model
model =
    { count = 0, inc = 1, incString = "1", incError = "", view = MainView }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | Set100
    | Add5
    | SetInc String
    | ToggleView View
    | SetView View


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = (+) model.count model.inc }

        Decrement ->
            { model | count = (-) model.count model.inc }

        Reset ->
            { model | count = 0 }

        Set100 ->
            { model | count = 100 }

        Add5 ->
            { model | count = (+) model.count 5 }

        SetInc string ->
            case String.toInt string of
                Ok v ->
                    { model | incString = string, inc = v, incError = "" }

                Err e ->
                    { model | incString = string, incError = e }

        ToggleView v ->
            { model | view = v }

        SetView v ->
            { model | view = v }


b : Msg -> String -> Html Msg
b msg txt =
    Html.button [ Html.Events.onClick msg ] [ Html.text txt ]


bb : Msg -> String -> Element MyStyles variation Msg
bb msg txt =
    row Button
        [ Element.Attributes.height (Element.Attributes.px 20), Element.Events.onClick msg ]
        [ Element.text <| toString msg ]


m : List View
m =
    [ MainView, ConfigView, EditView ]


x : Model -> Html Msg
x model =
    div []
        (List.map
            (\x -> span [] [ a [ Html.Events.onClick (SetView x) ] [ Html.text (toString x) ] ])
            m
        )


type Sign
    = Pos
    | Neg


type View
    = MainView
    | ConfigView
    | EditView


label : Sign -> Model -> String
label sign model =
    case sign of
        Pos ->
            "+" ++ toString model.inc

        Neg ->
            "-" ++ toString model.inc



-- VIEW


view1 : Model -> Html Msg
view1 model =
    div []
        ([ view2 model
         , view2 model
         ]
            ++ more model
            ++ more model
        )


more : Model -> List (Html Msg)
more model =
    [ x model
    , Html.button [ Html.Events.onClick Decrement ] [ Html.text ("-" ++ toString model.inc) ]
    , div [] [ Html.text (toString model.count ++ " " ++ String.repeat (abs model.count) "*") ]
    , Html.button [ Html.Events.onClick Increment ] [ Html.text <| "+" ++ toString model.inc ]
    , b Set100 "set100"
    , b Decrement (label Neg model)
    , b Increment <| label Pos model
    , b Add5 "add 5"
    , b Reset "reset!"
    , input [ Html.Events.onInput SetInc, value model.incString ] []
    , p [] [ Html.text <| toString model ]
    ]


view : Model -> Html Msg
view model =
    Element.layout stylesheet <| pageWrapper model


pageWrapper : Model -> Element MyStyles variation Msg
pageWrapper model =
    row None
        [ padding 20
        , paddingTop 5
        , Element.Attributes.paddingBottom 20
        ]
        [ pageArea model ]


pageArea : Model -> Element MyStyles variation Msg
pageArea model =
    column None
        [ Element.Attributes.width (Element.Attributes.percent 100) ]
        [ el None
            [ Element.Attributes.center ]
            (Element.text <| "Header " ++ toString model)
        , row None
            []
            [ bb Reset "reset!"
            , bb Set100 "Set 100"
            , bb Add5 "Add 5"

            -- , Element.Input.text
            --     None
            --     []
            --     Element.Input.Text
            --     { onChange = SetInc
            --     , value = model.incString
            --     , label =
            --         Element.Input.placeholder
            --             { label = Element.Input.labelLeft (Element.el None [] empty)
            --             , text = Element.Input.Text "Placeholder!"
            --             }
            --     , options = []
            --     }
            ]
        , footerArea model
        ]


footerArea : Model -> Element MyStyles variation Msg
footerArea model =
    el None
        [ Element.Attributes.center ]
        (Element.text <| "Footer " ++ toString model)


viewx : Model -> Html Msg
viewx model =
    case model.view of
        MainView ->
            div []
                [ view1 model, view1 model ]

        ConfigView ->
            view2 model

        EditView ->
            view3 model


view3 : Model -> Html Msg
view3 model =
    layout stylesheet <|
        Element.row
            None
            []
            ([ Element.text <| toString model
             , Element.text (toString model)
             , toString model |> Element.text
             ]
                ++ z m model
            )


view2 : Model -> Html Msg
view2 model =
    layout stylesheet <|
        Element.row
            None
            [ spacing 10 ]
            ([ Element.text (toString model) ]
                ++ z m model
            )


z : List View -> Model -> List (Element MyStyles variation Msg)
z viewlist model =
    List.map
        (\v ->
            if v /= model.view then
                Element.link "#" <| el None [ Element.Events.onClick <| SetView v ] (Element.text <| toString v)
            else
                el None [] (Element.text <| toString v ++ "!!")
        )
        viewlist
