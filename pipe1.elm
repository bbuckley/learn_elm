module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


x : List Int
x =
    [ 1, 7, 43, 104, 5, 3, 4, 43, 5, 34, 101, 50, 51, 49 ]


yy : List (Maybe Int)
yy =
    [ Just 1, Just 7, Nothing, Just 43, Nothing, Just 104, Just 5, Just 3, Just 4, Just 43 ]


type alias Person =
    { id : String
    , age : Int
    , pbc : Maybe String
    , stat : Stat
    , status : Maybe Stat
    , dob : String
    }


type Stat
    = A
    | D
    | T


persons : List Int -> List Person
persons list =
    List.map (\i -> Person "x" i Nothing A Nothing "") list


personList : List Person
personList =
    [ { id = "1", age = 44, pbc = Just "Brian", stat = A, status = Nothing, dob = "" }
    , { id = "12", age = 45, pbc = Just "Brian", stat = A, status = Just A, dob = "" }
    , { id = "22", age = 45, pbc = Just "Brian", stat = A, status = Nothing, dob = "" }
    , { id = "2", age = 45, pbc = Just "Brian", stat = A, status = Nothing, dob = "" }
    , { id = "3", age = 34, pbc = Nothing, stat = A, status = Nothing, dob = "" }
    , { id = "4", age = 55, pbc = Just "David", stat = A, status = Nothing, dob = "" }
    , { id = "5", age = 45, pbc = Just "Brian", stat = D, status = Just D, dob = "" }
    , { id = "6", age = 23, pbc = Nothing, stat = A, status = Nothing, dob = "" }
    , { id = "7", age = 24, pbc = Nothing, stat = A, status = Nothing, dob = "" }
    , { id = "8", age = 26, pbc = Just "David", stat = A, status = Nothing, dob = "" }
    , { id = "9", age = 25, pbc = Just "Brian", stat = A, status = Nothing, dob = "" }
    , { id = "10", age = 30, pbc = Nothing, stat = A, status = Nothing, dob = "" }
    ]


type alias Model =
    { personList : List Person
    , filter : List Int
    , editing : Maybe String
    , fld : Maybe String
    , srt : String
    }


initModel : Model
initModel =
    { personList = personList
    , filter = out
    , editing = Nothing
    , fld = Nothing
    , srt = "id"
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Sort s ->
            model

        Orig ->
            initModel

        MarkField ( id, field ) ->
            { model | fld = Just field, editing = Just id }

        Mark id ->
            { model | editing = Just id }

        NoMark ->
            { model | editing = Nothing }

        Delete id ->
            { model
                | personList = List.filter (\x -> x.id /= id) model.personList
            }

        DeleteMarked ->
            case model.editing of
                Nothing ->
                    model

                Just id ->
                    { model
                        | editing = Nothing
                        , personList = List.filter (\x -> x.id /= id) model.personList
                    }


type Msg
    = Mark String
    | Sort String
    | Orig
    | NoMark
    | DeleteMarked
    | Delete String
    | MarkField ( String, String )


out : List Int
out =
    [ 111, 777, 49, 101 ]


xxx : List Int
xxx =
    x
        |> List.filter (\x -> (/=) x 7)
        |> List.filter (\x -> (/=) x 43)
        |> List.filter (\x -> (<) x 50)
        |> List.filter (\x -> not (List.member x out))



--age = Person ->


sort model =
    case model.srt of
        Nothing ->
            \x -> x

        _ ->
            \x -> x


filtered : Model -> List Person
filtered model =
    model.personList
        |> List.filter (\p -> (>) p.age 30)
        |> List.filter (\p -> not (List.member p.age out))
        |> List.filter (\p -> (/=) p.pbc Nothing)
        |> List.filter (\p -> (||) ((/=) p.pbc Nothing) ((==) p.age 45))
        |> List.filter (\p -> not ((&&) ((>=) p.age 40) ((<) p.age 10)))
        |> List.sort (sort model)
        |> List.sortBy .age


st : List ( String, String )
st =
    [ ( "border", "dotted" ), ( "color", "red" ) ]


td_age : Person -> Html msg
td_age person =
    td [] [ text (toString person.age) ]


header : Model -> Html Msg
header model =
    thead []
        (List.map (\x -> th [ onClick (Sort x) ] [ text x ])
            [ "", "id", "age", "pbc", "stat", "status", "dob" ]
        )


person : Person -> Html Msg
person x =
    tr []
        [ td [ onClick (Delete x.id) ] [ text "[x]" ]
        , td [ onClick (MarkField ( x.id, "id" )) ] [ text x.id ]
        , td [ onClick (MarkField ( x.id, "age" )) ] [ text (toString x.age) ]
        , td [ onClick (MarkField ( x.id, "pbc" )) ] [ text (toString x.pbc) ]
        , td [ onClick (MarkField ( x.id, "stat" )) ] [ text (toString x.stat) ]
        , td [ onClick (MarkField ( x.id, "status" )) ] [ text (toString x.status) ]
        , td [ onClick (MarkField ( x.id, "dob" )) ] [ text (toString x.dob) ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


viewMenu : Model -> Html Msg
viewMenu model =
    div []
        [ p []
            [ a [ onClick NoMark ] [ text " [Unmark] " ]
            , a [ onClick DeleteMarked ] [ text " [DeleteMarked] " ]
            , a [ onClick Orig ] [ text " [Orig] " ]
            ]
        ]


viewRows : Model -> List (Html Msg)
viewRows model =
    List.map (\x -> person x) (filtered model)


viewEditing : Model -> Html Msg
viewEditing model =
    p [] [ text "todo" ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [] [ p [] [ text "footer" ] ]


view : Model -> Html Msg
view model =
    div
        []
        [ viewMenu model
        , p [] [ text (toString model) ]
        , viewEditing model
        , table [ style st ] (header model :: viewRows model)
        , viewFooter model
        ]
