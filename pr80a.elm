module Main exposing (..)

import Html exposing (..)
import Set


type alias Edge comparable =
    ( comparable, comparable )


type alias AdjList comparable =
    List ( comparable, List comparable )


type alias Graph comparable =
    ( List comparable, List (Edge comparable) )


graphToAdjList : Graph comparable -> AdjList comparable
graphToAdjList ( nodes, edges ) =
    List.map (\n -> ( n, edgesOf n edges )) nodes


edgesOf : comparable -> List ( comparable, comparable ) -> List comparable
edgesOf n edges =
    let
        ( xs, ys ) =
            List.unzip <|
                List.filter (\( x, y ) -> x == n || y == n) edges
    in
    List.sort <|
        Set.toList <|
            Set.fromList <|
                List.filter (\x -> x /= n) <|
                    xs
                        ++ ys


main : Html msg
main =
    text
        (if test then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ adjList80 == graphToAdjList graph80
        , [] == graphToAdjList ( [], [] )
        ]


graph80 : ( List Char, List ( Char, Char ) )
graph80 =
    ( [ 'b', 'c', 'd', 'f', 'g', 'h', 'k' ]
    , [ ( 'b', 'c' ), ( 'b', 'f' ), ( 'c', 'f' ), ( 'f', 'k' ), ( 'g', 'h' ) ]
    )


adjList80 : List ( Char, List Char )
adjList80 =
    [ ( 'b', [ 'c', 'f' ] )
    , ( 'c', [ 'b', 'f' ] )
    , ( 'd', [] )
    , ( 'f', [ 'b', 'c', 'k' ] )
    , ( 'g', [ 'h' ] )
    , ( 'h', [ 'g' ] )
    , ( 'k', [ 'f' ] )
    ]
