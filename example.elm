module Example exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Paginate exposing (PaginatedList)


--exposing (..)


type alias Ee =
    { info : String
    , foo : Int
    }


type alias Model =
    { things : PaginatedList String
    , reversed : Bool
    , query : String
    , globalId : Int
    , ees : PaginatedList Ee
    }


type Msg
    = Next
    | Prev
    | First
    | Last
    | GoTo Int
    | ChangePageSize String
    | DeleteItem String
    | AddItem
    | Reverse
    | Find String
    | AddItem10


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init

        -- , view = filterAndSortThings >> view
        , view = view
        , update = update
        }


init : Model
init =
    let
        things : List String
        things =
            List.map (toString >> (++) "item ") <| List.range 1 37
    in
    { things = Paginate.fromList 10 things
    , reversed = False
    , query = ""
    , globalId = List.length things
    , ees = Paginate.fromList 2 [ Ee "test" 1, Ee "test" 2 ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoTo index ->
            { model | things = Paginate.goTo index model.things }

        Next ->
            { model | things = Paginate.next model.things }

        Prev ->
            { model | things = Paginate.prev model.things }

        First ->
            { model | things = Paginate.first model.things }

        Last ->
            { model | things = Paginate.last model.things }

        ChangePageSize size ->
            let
                sizeAsInt =
                    Result.withDefault 10 <| String.toInt size
            in
            { model | things = Paginate.changeItemsPerPage sizeAsInt model.things }

        DeleteItem item ->
            { model | things = Paginate.map (List.filter ((/=) item)) model.things }

        AddItem ->
            let
                newId =
                    model.globalId + 1

                addItem existing =
                    existing ++ [ "new item " ++ toString newId ]
            in
            { model
                | things = Paginate.map addItem model.things
                , globalId = newId
            }

        AddItem10 ->
            List.foldr update model (List.map (\_ -> AddItem) (List.range 1 10))

        Reverse ->
            { model | reversed = not model.reversed }

        Find query ->
            { model | query = query }


view : Model -> Html Msg
view model =
    viewp (filterAndSortThings model)


filterAndSortThings : Model -> PaginatedList String
filterAndSortThings model =
    let
        sort =
            if model.reversed then
                List.reverse
            else
                identity

        filter =
            if model.query == "" then
                identity
            else
                List.filter (\thing -> String.contains model.query (toString thing))
    in
    Paginate.map (filter >> sort) model.things


viewp : PaginatedList String -> Html Msg
viewp filteredSortedThings =
    let
        displayInfoView =
            div []
                [ div []
                    [ text <|
                        String.join " " <|
                            [ "showing"
                            , toString <| List.length <| Paginate.page filteredSortedThings
                            , "of"
                            , toString <| Paginate.length filteredSortedThings
                            , "items"
                            ]
                    , u [ onClick <| AddItem, style [ ( "cursor", "pointer" ) ] ] [ text " (add more)" ]
                    , u [ onClick <| AddItem10, style [ ( "cursor", "pointer" ) ] ] [ text " (add 10 more)" ]
                    ]
                , text <|
                    String.join " "
                        [ "page"
                        , toString <| Paginate.currentPage filteredSortedThings
                        , "of"
                        , toString <| Paginate.totalPages filteredSortedThings
                        ]
                , div []
                    [ text <|
                        String.join " "
                            [ "including"
                            , Paginate.foldMap
                                (List.filter (String.contains "new item") >> List.length >> toString)
                                filteredSortedThings
                            , "new items"
                            ]
                    ]
                ]

        itemView item =
            li []
                [ span [] [ text item ]
                , u [ onClick <| DeleteItem item, style [ ( "cursor", "pointer" ) ] ] [ text " (delete)" ]
                ]

        itemsPerPageSelector =
            div []
                [ text "show"
                , select [ onInput ChangePageSize ]
                    [ option [ value "10" ] [ text "10" ]
                    , option [ value "20" ] [ text "20" ]
                    , option [ value "30" ] [ text "30" ]
                    ]
                , text "items per page"
                ]
    in
    div [] <|
        [ displayInfoView
        , itemsPerPageSelector
        , button [ onClick Reverse ] [ text "Reverse list" ]
        , input [ placeholder "Search...", onInput Find ] []
        , foot filteredSortedThings
        , ul [] (List.map itemView <| Paginate.page filteredSortedThings)
        , foot filteredSortedThings
        ]


foot : PaginatedList a -> Html Msg
foot filteredSortedThings =
    let
        prevButtons =
            [ button [ onClick First, disabled <| Paginate.isFirst filteredSortedThings ] [ text "<<" ]
            , button [ onClick Prev, disabled <| Paginate.isFirst filteredSortedThings ] [ text "<" ]
            ]

        nextButtons =
            [ button [ onClick Next, disabled <| Paginate.isLast filteredSortedThings ] [ text ">" ]
            , button [ onClick Last, disabled <| Paginate.isLast filteredSortedThings ] [ text ">>" ]
            ]

        pagerButtonView index isActive =
            button
                [ style
                    [ ( "font-weight"
                      , if isActive then
                            "bold"
                        else
                            "normal"
                      )
                    ]
                , onClick <| GoTo index
                ]
                [ text <| toString index ]
    in
    div [] (prevButtons ++ [ span [] <| Paginate.pager pagerButtonView filteredSortedThings ] ++ nextButtons)
