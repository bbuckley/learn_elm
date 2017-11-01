module Main exposing (..)

import Html exposing (..)
import String


-- foo : List number


foo : List number
foo =
    [ 3, 1, 3, 5, 6, 7 ]


rrr : String
rrr =
    String.repeat 3 "x"


y : Int -> String
y n =
    String.repeat n "y"


xxx n =
    String.repeat n "s"


aaaa : Int -> Float -> c -> String
aaaa a b c =
    "test"


aaaaa : Float -> c -> String
aaaaa =
    aaaa 8888



-- x : List String


x =
    -- List.map (\x -> String.repeat x "x") foo
    -- List.map y foo
    List.map xxx foo


main : Html msg
main =
    text (toString x)
