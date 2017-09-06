module Pr23 exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Random


randomSelect : Random.Seed -> Int -> List a -> ( List a, Random.Seed )
randomSelect seed n list =
    let
        ( l, r, s ) =
            randSelect n ( [], list, seed )
    in
        ( l, s )


randSelect : Int -> ( List a, List a, Random.Seed ) -> ( List a, List a, Random.Seed )
randSelect n ( l, r, seed ) =
    if n > 0 then
        let
            ( idx, seed_ ) =
                Random.step (Random.int 1 (List.length r)) seed

            e =
                elementAt r idx

            r_ =
                dropAt r idx
        in
            case e of
                Nothing ->
                    ( l, r, seed )

                Just x ->
                    randSelect (n - 1) ( x :: l, r_, seed_ )
    else
        ( l, r, seed )


elementAt : List a -> Int -> Maybe a
elementAt list n =
    case List.drop (n - 1) list of
        [] ->
            Nothing

        y :: ys ->
            Just y


dropAt : List a -> Int -> List a
dropAt list n =
    case list of
        [] ->
            []

        x :: xs ->
            (List.take (n - 1) list) ++ (List.drop n list)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { intSeed : Int
    , tested : Bool
    , passed : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1 False False, Cmd.none )



-- UPDATE


type Msg
    = Test
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test ->
            ( model, Random.generate NewFace (Random.int Random.minInt Random.maxInt) )

        NewFace newSeed ->
            ( Model newSeed True (test model.intSeed), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (testMsg model.tested model.passed) ]
        , p [] [ text ("Seed value: " ++ (toString (model.intSeed))) ]
        , p [] [ text ("Your die roll is " ++ (toString (Tuple.first (randomSelect (Random.initialSeed model.intSeed) 1 (1..6))))) ]
        , button [ onClick Test ] [ text "Test" ]
        ]


test : Int -> Bool
test intSeed =
    let
        seed =
            Random.initialSeed intSeed

        ( l1, s1 ) =
            randomSelect seed 3 (1..1000)

        ( l2, s2 ) =
            randomSelect seed 3 (1..1000)

        ( l3, s3 ) =
            randomSelect s2 3 (1..1000)

        ( l4, s4 ) =
            randomSelect s3 9 (1..9)

        ( l5, s5 ) =
            randomSelect s4 3 [ "a", "b" ]

        ( l6, s6 ) =
            randomSelect s5 0 [ 'a', 'b' ]

        ( l7, s7 ) =
            randomSelect s6 -1 [ 'a', 'b' ]

        ( l8, s8 ) =
            randomSelect s7 1 [ 'a', 'b' ]
    in
        List.all ((==) True)
            [ List.sort l1 == List.sort l2
            , l1 == l2
            , l2 /= l3
              -- a billion to one that this won't match
            , List.sort l4 == 1..9
            , List.sort l5 == [ "a", "b" ]
            , l6 == []
            , l7 == []
            ]


testMsg : Bool -> Bool -> String
testMsg tested passed =
    if tested then
        if passed then
            "Your implementation passed all tests."
        else
            "Your implementation failed at least one test."
    else
        "Click the test button below"



(..) : Int -> Int -> List Int
(..) start end =
    List.range start end
