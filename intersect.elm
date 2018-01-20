module Main exposing (..)

import Html exposing (Html, text)
import Set exposing (Set)


ll : List (List Int)
ll =
    [ [ 1, 3, 2 ], [ 2, 3, 4, 1 ], [ 2, 22, 33, 1, 7 ] ]



-- ll =
--     [ [ 1, 3, 2 ] ]


intersectAll5 : List (List comparable) -> List comparable
intersectAll5 =
    List.concat >> Set.fromList >> Set.toList


intersectAll4 : List (List comparable) -> List comparable
intersectAll4 l =
    List.concat l |> Set.fromList |> Set.toList


intersectAll3 : List (List comparable) -> List comparable
intersectAll3 lists =
    Set.fromList (List.concat lists) |> Set.toList


intersectAll2 : List (List comparable) -> List comparable
intersectAll2 lists =
    case lists of
        [] ->
            []

        first :: rest ->
            Set.fromList (List.concat (first :: rest)) |> Set.toList


intersectAll : List (List comparable) -> List comparable
intersectAll lists =
    case lists of
        [] ->
            []

        first :: rest ->
            List.foldl (\list acc -> Set.intersect (Set.fromList list) acc) (Set.fromList first) rest
                |> Set.toList


main : Html msg
main =
    --text (toString (intersectAll ll))
    ll |> intersectAll |> toString |> text



--text (toString ([ [ 1, 6, 6 ], [ 6, 7 ], [ 1 ] ] |> List.concat |> Set.fromList |> Set.toList))
--ll |> List.concat |> Set.fromList |> Set.toList |> toString |> text
-- text (toString ll)
