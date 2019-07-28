module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Browser exposing (sandbox)


type alias Tc =
    { calc_type : String, id : String, pbc : String, crd : String }


type Field
    = Pbc
    | Dob
    | Crd


fields : List Field
fields =
    [ Pbc, Crd, Dob ]


a : List Tc
a =
    [ { id = "1", calc_type = "Modeling", pbc = "Brian", crd = "11/12/1958" }
    , { id = "2", calc_type = "Final", pbc = "Brian", crd = "11/12/1958" }
    , { id = "33", calc_type = "Modeling", pbc = "David", crd = "11/12/1958" }
    , { id = "4", calc_type = "UI1", pbc = "David", crd = "11/12/1958" }
    , { id = "5", calc_type = "UI1", pbc = "Karl", crd = "12/2/1960" }
    ]


type alias Model =
    { interest : Float
    , interestInput : String
    , interestInputErrorMessage : Maybe String
    , filter : Maybe String
    , tcid : Maybe String
    , sort : String
    , tcs : List Tc
    }


initModel : Model
initModel =
    Model 0.08 "0.08" Nothing (Just "pbc") Nothing "pbc" a



main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view }


type Msg
    = InterestInput String
    | InterestEnter
    | Pick String
    | Sort String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InterestEnter ->
            case String.toFloat model.interestInput of
                
                
                
                Ok value ->
                    { model | interestInput = model.interestInput, interest = value, interestInputErrorMessage = Nothing }

                Err error ->
                    { model | interestInputErrorMessage = Just error }

        InterestInput x ->
            { model | interestInput = x }

        Pick id ->
            { model | tcid = Just id }

        Sort label ->
            { model | sort = label }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


viewErr : Maybe String -> String
viewErr s =
    case s of
        Nothing ->
            ""

        Just x ->
            x


renderTc : Tc -> Html Msg
renderTc tc =
    text (.id tc ++ " " ++ .calc_type tc ++ " " ++ .pbc tc)


renderList : List Tc -> Html Msg
renderList tcs =
    ul []
        (List.map (\tc -> li [ onClick (Pick tc.id) ] [ renderTc tc ]) tcs)


renderHeaders : List String -> Html Msg
renderHeaders hs =
    tr [] (List.map (\header -> th [ onClick (Sort header) ] [ text header ]) hs)


renderRow : Html Msg
renderRow =
    tr [] [ td [] [ text "foobar" ] ]


renderTd : Html Msg
renderTd =
    td [] [ text "foobar" ]


headers : List String
headers =
    [ "id", "pbc", "calc_type", "crd" ]


renderTable : List Tc -> Html Msg
renderTable tc =
    table [ style  "color" "red" , style "border" "solid" ] 
        [ renderHeaders headers
        , renderRow
        , tr []
            [ td []
                [ text "foo" ]
            , td
                []
                [ text "foo" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ input [ onEnter InterestEnter, onInput InterestInput, value model.interestInput ] []
        , button [ onClick InterestEnter ] [ text "Interest" ]
        , p [] [ text (viewErr model.interestInputErrorMessage) ]
        , div []
            [ br [] []
            , renderList a
            , renderTable a
            ]
        -- , p [] [ text (toString (viewTc model)) ]
        ]


viewTc : Model -> Maybe Tc
viewTc model =
    case model.tcid of
        Nothing ->
            Nothing

        Just x ->
            List.head (List.filter (\tc -> tc.id == x) model.tcs)
