module Pipeline exposing (main)

import Html exposing (Html, text)
import Ordering exposing (..)


a : List Int
a =
    [ 2, 28, 4, -9 ]


tuple : Int -> ( Int, Int, Int )
tuple =
    \x -> ( x, x * x, -x + 4 )


type alias A =
    { foo : Int
    , bar : Int
    , baz : Maybe Int
    }


toA : ( Int, Int, Int ) -> A
toA =
    \( x, y, z ) ->
        { foo = x
        , bar = y
        , baz =
            if z == 0 then
                Just 88
            else
                Just z
        }


b : List ( Int, Int, Int )
b =
    List.map tuple a


c : List A
c =
    List.map toA b


main : Html msg
main =
    c
        |> List.sortWith (Ordering.byField .foo)
        |> List.reverse
        |> List.reverse
        |> toString
        |> Html.text