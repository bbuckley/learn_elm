module Pipeline exposing (main)

import Html exposing (Html, text)
import Ordering exposing (byField)


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


-- type alias AA =
--     { foo : Int
--     , bar : Int
--     , baz : Int
--     }


-- atoAA : A -> AA
-- atoAA aa =
--     AA
--         aa.foo
--         aa.bar
--         (Maybe.withDefault 0 aa.baz)


aToS : A -> String
aToS { foo, bar } =
    List.map String.fromInt [ foo, bar ] |> String.join ","


toA : ( Int, Int, Int ) -> A
toA =
    \( x, y, z ) ->
        { foo = x
        , bar = y
        , baz =
            if z == 0 then
                Nothing

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
        |> List.map aToS
        |> String.join ","
        |> Html.text
