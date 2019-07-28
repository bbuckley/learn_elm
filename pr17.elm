module Main exposing (main)

import Html exposing (Html, text)


split : List a -> Int -> ( List a, List a )
split list count =
    ( List.take count list, List.drop count list )


main : Html msg
main =
    text
        (if test then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )




a : List ( ( List Int, List Int ), ( List Int, List Int ) )





a =
    [ ( split (List.range 1 5) 0, ( [], [ 1, 2, 3, 4, 5 ] ) )
    , ( split (List.range 1 5) 2, ( [ 1, 2 ], [ 3, 4, 5 ] ) )
    , ( split (List.range 1 5) 3, ( [ 1, 2, 3 ], [ 4, 5 ] ) )
    , ( split (List.range 1 5) 4, ( [ 1, 2, 3, 4 ], [ 5 ] ) )
    , ( split (List.range 1 5) 5, ( [ 1, 2, 3, 4, 5 ], [] ) )
    , ( split (List.range 1 5) 6, ( [ 1, 2, 3, 4, 5 ], [] ) )
    , ( split (List.range 1 5) -1, ( [], [ 1, 2, 3, 4, 5 ] ) )

    -- , ( split [ 1.1, 2.2, 3.3, 4.5 ] -1, ( [], [ 1.1, 2.2, 3.3, 4.5 ] ) )
    ]


b : List ( ( List String, List String ), ( List String, List String ) )
b =
    [ ( split [ "a", "az", "aaa" ] 2, ( [ "a", "az" ], [ "aaa" ] ) )
    ]


test : Bool
test =
    List.all (\( result, expect ) -> result == expect) b
