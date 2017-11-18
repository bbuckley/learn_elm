module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Paginate exposing (PaginatedList, fromList, goTo, next, page, pager)
import Presidents exposing (presidents)


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
    { pageno : Int }


initModel =
    { pageno = 5 }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GoTo pageno ->
            { model | pageno = pageno }


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
        [ text ("TC" ++ toString index) ]


type Msg
    = GoTo Int


xxx : PaginatedList Int
xxx =
    fromList 1 xx


pagerView : Model -> Html Msg
pagerView model =
    div [] <|
        pager (\pageNo isCurPg -> renderPagerButton pageNo isCurPg) (goTo model.pageno xxx)


view : Model -> Html Msg
view model =
    div []
        [ pagerView model
        , p [] [ xxx |> goTo model.pageno |> page |> toString |> text ]
        , p [] [ model |> toString |> text ]
        , p [] [ (List.head presidents |> toString) ++ " blah" |> text ]
        , model |> pagerView
        ]
