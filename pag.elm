module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onClick, onInput)
import Paginate exposing (PaginatedList, fromList, goTo, next, page, pager)
import Presidents exposing (Person, presidents)


xx =
    List.range 1 100



-- -- main =
-- pagerView


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }


type alias Model =
    { pageno : Int
    , query : String
    , list : PaginatedList Person
    }


initModel : Model
initModel =
    { pageno = 5, query = "", list = fromList 5 presidents }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoTo pageno ->
            { model | pageno = pageno }

        SetQuery query ->
            { model | query = query }


x : List Int
x =
    -- equals [ 21, 22, 23, 24, 25, 26, 27, 28, 29 30 ]
    xx |> fromList 10 |> goTo 3 |> page


y : List Int
y =
    page <| goTo 3 <| fromList 10 <| xx


z =
    fromList 10 <| xx


a =
    fromList 2 xx
        |> next
        |> pager (,)


renderPagerButton : Int -> Bool -> Html Msg
renderPagerButton index isActive =
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
        -- [ text <| toString index ]
        [ text (toString index) ]


type Msg
    = GoTo Int
    | SetQuery String


xxx : PaginatedList Person
xxx =
    fromList 5 presidents



-- filteredModel : Model -> PaginatedList
-- filteredModel model =
--     PaginatedList.filter (String.contains model.query << String.toLower << .name) model.list


filteredModel1 : Model -> List Person -> List Person
filteredModel1 model person =
    List.filter (String.contains model.query << String.toLower << .name) person


pp : Model -> List Person
pp model =
    model.list |> goTo model.pageno |> page


items : Model -> List (Html msg)
items model =
    List.map (\x -> li [] [ text (toString x) ]) (pp model)



--xx


pagerView : Model -> Html Msg
pagerView model =
    div [] <|
        pager (\pageNo isCurPg -> renderPagerButton pageNo isCurPg) (goTo model.pageno model.list)


view : Model -> Html Msg
view model =
    div []
        [ pagerView model
        , p [] [ xxx |> goTo model.pageno |> page |> toString |> text ]
        , p [] [ model |> toString |> text ]
        , p [] [ (List.head presidents |> toString) ++ " blah" |> text ]
        , model |> pagerView
        , input [ placeholder "Search by Name", onInput SetQuery ] []
        ]
