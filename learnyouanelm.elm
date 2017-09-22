module Main exposing (..)

import Html


main =
    Html.text (toString toPrint)


doubleMe x =
    x + x


doubleSmallNumber x =
    if x > 100 then
        x
    else
        x * 2


doubleSmallNumber_ x =
    (if x > 100 then
        x
     else
        x * 2
    )
        + 1


toPrint =
    [ ( 1, 2 ), ( 8, 11, 5 ), ( 4, 5 ) ]
