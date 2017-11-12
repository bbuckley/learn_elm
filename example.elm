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

    -- , ees : List Ee
    }


type Msg
    = Next
    | Prev
    | First
    | Last
    | GoTo Int
    | GoToEe Int
    | ChangePageSize String
    | DeleteItem String
    | AddItem
    | Reverse
    | Find String
    | AddItem10
    | AddEe
    | AddEe5
    | ChangePageSizeEe String
    | NextEe
    | PrevEe
    | FirstEe
    | LastEe


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
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
    , ees = Paginate.fromList 5 [ Ee "test" 100, Ee "test" 200 ]

    -- , ees = List.map identity [ Ee "test" 1, Ee "test" 2 ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoTo index ->
            { model | things = Paginate.goTo index model.things }

        GoToEe indexEe ->
            { model | ees = Paginate.goTo indexEe model.ees }

        Next ->
            { model | things = Paginate.next model.things }

        NextEe ->
            { model | ees = Paginate.next model.ees }

        Prev ->
            { model | things = Paginate.prev model.things }

        First ->
            { model | things = Paginate.first model.things }

        Last ->
            { model | things = Paginate.last model.things }

        PrevEe ->
            { model | ees = Paginate.prev model.ees }

        FirstEe ->
            { model | ees = Paginate.first model.ees }

        LastEe ->
            { model | ees = Paginate.last model.ees }

        ChangePageSize size ->
            let
                sizeAsInt =
                    Result.withDefault 10 <| String.toInt size
            in
            { model | things = Paginate.changeItemsPerPage sizeAsInt model.things }

        ChangePageSizeEe size ->
            let
                sizeAsInt =
                    Result.withDefault 5 <| String.toInt size
            in
            { model | ees = Paginate.changeItemsPerPage sizeAsInt model.ees }

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

        AddEe ->
            let
                newId =
                    model.globalId + 1

                addItem existing =
                    existing ++ [ Ee "new item " newId ]
            in
            { model
                | ees = Paginate.map addItem model.ees
                , globalId = newId
            }

        AddEe5 ->
            List.foldr update model (List.map (\_ -> AddEe) (List.range 1 5))


view : Model -> Html Msg
view model =
    -- model |> filterAndSortThings |> viewp
    let
        x =
            viewp <| filterAndSortThings <| model
    in
    div []
        [ div [] [ x ]
        , br [] []

        -- , ul [] (Paginate.map (\x -> li [] [ text "ee" ]) model.ees)
        , viewpEe <| filterAndSortEes <| model
        , div [] [ text (toString model) ]
        ]


filterAndSortEes : Model -> PaginatedList Ee
filterAndSortEes model =
    let
        sort : List Ee -> List Ee
        sort =
            if model.reversed then
                List.reverse
            else
                identity

        filter : List Ee -> List Ee
        filter =
            if model.query == "" then
                identity
            else
                List.filter (\ee -> String.contains model.query (toString ee))
    in
    Paginate.map (filter >> sort) model.ees


filterAndSortThings : Model -> PaginatedList String
filterAndSortThings model =
    let
        sort : List String -> List String
        sort =
            if model.reversed then
                List.reverse
            else
                identity

        filter : List String -> List String
        filter =
            if model.query == "" then
                identity
            else
                List.filter (\thing -> String.contains model.query (toString thing))
    in
    Paginate.map (filter >> sort) model.things


viewpEe : PaginatedList Ee -> Html Msg
viewpEe filteredSortedEe =
    let
        itemViewEe : Ee -> Html Msg
        itemViewEe item =
            li []
                [ span [] [ text (toString item) ]
                , u [ style [ ( "cursor", "pointer" ) ] ] [ text " (delete)" ]
                ]

        itemsPerPageSelectorEe =
            div []
                [ text "show"
                , select [ onInput ChangePageSizeEe ]
                    [ option [ value "5" ] [ text "5" ]
                    , option [ value "15" ] [ text "15" ]
                    ]
                , text "items per page"
                ]
    in
    div [] <|
        [ itemsPerPageSelectorEe
        , button [ onClick Reverse ] [ text "Reverse list" ]
        , input [ placeholder "Search...", onInput Find ] []
        , foot filteredSortedEe
        , ul [] (List.map itemViewEe <| Paginate.page filteredSortedEe)
        ]


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
                    , u [ onClick <| AddEe, style [ ( "cursor", "pointer" ) ] ] [ text " (add Ee)" ]
                    , u [ onClick <| AddEe5, style [ ( "cursor", "pointer" ) ] ] [ text " (add 5 Ee)" ]
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

        itemView : String -> Html Msg
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

                -- , onClick (GoTo index)
                , onClick <| GoTo index
                ]
                [ text <| toString index ]
    in
    div [] (prevButtons ++ [ span [] <| Paginate.pager pagerButtonView filteredSortedThings ] ++ nextButtons)


footEe : PaginatedList Ee -> Html Msg
footEe filteredSortedEe =
    let
        prevButtons =
            [ button [ onClick FirstEe, disabled <| Paginate.isFirst filteredSortedEe ] [ text "<<" ]
            , button [ onClick PrevEe, disabled <| Paginate.isFirst filteredSortedEe ] [ text "<" ]
            ]

        nextButtons =
            [ button [ onClick NextEe, disabled <| Paginate.isLast filteredSortedEe ] [ text ">" ]
            , button [ onClick LastEe, disabled <| Paginate.isLast filteredSortedEe ] [ text ">>" ]
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

                -- , onClick (GoTo index)
                , onClick <| GoToEe index
                ]
                [ text <| toString index ]
    in
    div [] (prevButtons ++ [ span [] <| Paginate.pager pagerButtonView filteredSortedEe ] ++ nextButtons)
