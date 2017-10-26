module Combobox2 exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as K exposing (checkbox)
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
    | N999


rics : List Ric
rics =
    [ EXE, SUB, BANK, N999 ]


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


type alias Model =
    { title : Title
    , firstName : String
    , lastName : String
    , foobar : FooBar
    , suit : Suit
    , msuit : Maybe Suit
    , mRic : Maybe Ric
    , done : Bool
    , ricSet : Set String --really s/b Set Ric
    }


type Msg
    = NewTitle Title
    | NewFirstName String
    | NewLastName String
    | NewFooBar FooBar
    | NewSuit Suit
    | NewMSuit (Maybe Suit)
    | NewDone Bool
    | ToggleRic Bool --want the to be Ric


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

        NewDone b ->
            { model | done = b }

        ToggleRic ricString ->
            model


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
        , checkbox [] (member (toString ric) model.ricSet) |> Html.map ToggleRic
        ]


viewRics : Model -> Html Msg
viewRics model =
    Html.div [] (List.map (\x -> viewRic x model) rics)


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
        , Html.label [] [ Html.text "Done" ]
        , checkbox [] model.done |> Html.map NewDone
        , viewSuits model
        , viewRics model
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
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
