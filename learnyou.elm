module Main exposing (..)

import Html exposing (..)


xxx : Int
xxx =
    11


applyTwice : (a -> a) -> a -> a
applyTwice f x =
    f (f x)


applyThrice : (a -> a) -> a -> a
applyThrice f x =
    f (f (f x))


double : Int -> Int
double x =
    2 * x


add7 : Int -> Int
add7 =
    (+) 7


add4 : Int -> Int
add4 x =
    x + 4


main : Html msg
main =
    applyThrice add4 5
        |> toString
        |> text
