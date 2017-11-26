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


main : Html msg
main =
    applyThrice double 5
        |> toString
        |> text
