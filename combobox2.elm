module Combobox2 exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes as Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Kintail.InputWidget as K
import Set exposing (Set, member)


type FooBar
    = Foo
    | Bar
    | Nil


type Suit
    = Hearts
    | Spades
    | Diamonds
    | Clubs


suits : List Suit
suits =
    [ Hearts, Spades, Diamonds, Clubs ]


addNothing : List a -> List (Maybe a)
addNothing list =
    Nothing :: List.map (\x -> Just x) list



-- Nothing :: List.map just list
-- Nothing :: List.map (Just x) list


type Title
    = Dr
    | Mrs
    | Ms
    | Mr
    | Master
    | Miss


type Ric
    = EXE
    | SUB
    | BANK
    | N998
    | N999


rics : List Ric
rics =
    [ EXE, SUB, BANK, N998, N999 ]



-- ricString : List String
-- ricString =
--     List.map toString rics


type LOAType
    = MED
    | LTD
    | MIL


type Event
    = TER
    | LOA (Maybe LOAType)
    | XFR


events : List Event
events =
    [ TER, XFR, LOA (Just MED), LOA Nothing, LOA (Just MIL) ]


type Stat
    = A
    | T
    | L
    | D
    | X


stats : List Stat
stats =
    [ A, T, L, D ]


statsString : List String
statsString =
    List.map toString stats


type alias Model =
    { title : Title
    , firstName : String
    , lastName : String
    , foobar : FooBar
    , suit : Suit
    , msuit : Maybe Suit
    , mRic : Maybe Ric
    , done : Bool
    , ricSet : Set String --really wanted Set Ric but has to be comparable?
    , selectedRics : Set String
    , hasLoa : Bool
    , mStat : Maybe Stat
    }


type Msg
    = NewTitle Title
    | NewFirstName String
    | NewLastName String
    | NewFooBar FooBar
    | NewSuit Suit
    | NewMSuit (Maybe Suit)
    | NewDone Bool
    | ToggleRic Bool --Ric --want the to be Ric
    | Select String
    | Deselect String
    | ChangeLoa
    | NewMStat (Maybe Stat)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewTitle newTitle ->
            { model | title = newTitle }

        NewFirstName newFirstName ->
            { model | firstName = newFirstName }

        NewLastName newLastName ->
            { model | lastName = newLastName }

        NewFooBar foobar ->
            { model | foobar = foobar }

        NewSuit suit ->
            { model | suit = suit }

        NewMSuit msuit ->
            { model | msuit = msuit }

        NewMStat mStat ->
            { model | mStat = mStat }

        NewDone b ->
            { model | done = b }

        ToggleRic ricString ->
            model

        Select ric ->
            { model | selectedRics = Set.insert ric model.selectedRics }

        Deselect ric ->
            { model | selectedRics = Set.remove ric model.selectedRics }

        ChangeLoa ->
            { model | hasLoa = not model.hasLoa }


toStringFooBar : FooBar -> String
toStringFooBar foobar =
    case foobar of
        Foo ->
            "Foo"

        Bar ->
            "Bar"

        Nil ->
            ""


toStringMaybe : Maybe a -> String
toStringMaybe a =
    case a of
        Just v ->
            toString v

        Nothing ->
            ""


viewSuit : Suit -> Model -> Html Msg
viewSuit suit model =
    Html.span []
        [ Html.label [ Attributes.for (toString suit) ] [ Html.text (toString suit) ]
        , K.radioButton [] suit model.suit |> Html.map NewSuit
        ]


viewSuits : Model -> Html Msg
viewSuits model =
    Html.div [] (List.map (\x -> viewSuit x model) suits)


viewRic : Ric -> Model -> Html Msg
viewRic ric model =
    Html.span []
        [ Html.label [ Attributes.for (toString ric) ] [ Html.text (toString ric) ]

        -- , checkbox [] (member (toString ric) model.ricSet) |> Html.map ToggleRic
        , K.checkbox [] (member (toString ric) model.ricSet) |> Html.map ToggleRic
        ]


viewRics : Model -> Html Msg
viewRics model =
    Html.div [] (List.map (\x -> viewRic x model) rics)


viewRics2 : Model -> Html Msg
viewRics2 model =
    Html.div []
        [ Html.h2 [] [ Html.text "RICS" ]
        , Html.fieldset [] (List.map (mycheckbox model.selectedRics) (List.map toString rics))
        ]


mycheckbox : Set String -> String -> Html Msg
mycheckbox selectedRics ric =
    let
        isChecked =
            Set.member ric selectedRics

        msg =
            if isChecked then
                Deselect ric
            else
                Select ric
    in
    label [ style [ ( "display", "block" ) ] ]
        [ input
            [ type_ "checkbox"
            , checked (Set.member ric selectedRics)
            , onClick msg

            -- , onClick (ChangeRic ric isChecked)
            ]
            []
        , Html.text ric
        ]


checkbox : String -> Bool -> Msg -> Html Msg
checkbox string bool msg =
    label [] [ input [ type_ "checkbox", checked bool, onClick msg ] [], text string ]



--


view : Model -> Html Msg
view model =
    Html.div []
        [ K.comboBox [] toString [ Dr, Mrs, Ms, Mr, Master, Miss ] model.title |> Html.map NewTitle
        , K.lineEdit [] model.firstName |> Html.map NewFirstName
        , K.lineEdit [] model.lastName |> Html.map NewLastName
        , K.comboBox [] toString [ Nil, Foo, Bar ] model.foobar |> Html.map NewFooBar
        , K.comboBox [] toStringFooBar [ Nil, Bar ] model.foobar |> Html.map NewFooBar
        , K.comboBox [] toString [ Hearts, Spades, Diamonds, Clubs ] model.suit |> Html.map NewSuit
        , K.comboBox [] toString suits model.suit |> Html.map NewSuit
        , K.comboBox [] toStringMaybe (addNothing suits) model.msuit |> Html.map NewMSuit
        , K.comboBox [] toStringMaybe (addNothing stats) model.mStat |> Html.map NewMStat

        -- , K.lineEdit [] (toString model.mStat) |> Html.map NewLastName
        , label [] [ text "Done" ]
        , K.checkbox [] model.done |> Html.map NewDone
        , label [] [ input [ type_ "checkbox", checked model.hasLoa, onClick ChangeLoa ] [], text "Has LOA" ]
        , label [] [ input [ type_ "checkbox", checked model.hasLoa, onClick ChangeLoa ] [], text "Has REH" ]
        , checkbox "Has loa??" model.hasLoa ChangeLoa
        , viewSuits model
        , viewRics model
        , viewRics2 model
        , checkbox "LOA" model.hasLoa ChangeLoa
        , Html.br [] []
        , Html.text (toString model)
        ]


initModel : Model
initModel =
    { title = Dr
    , firstName = "Albert"
    , lastName = "Einstien"
    , foobar = Bar
    , suit = Diamonds
    , msuit = Nothing
    , mRic = Nothing
    , done = False
    , ricSet = Set.fromList [ "N999", "BANK" ]
    , selectedRics = Set.empty
    , hasLoa = False
    , mStat = Nothing
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
