module Selected exposing (cycle, fromList, fromLists, selected)

import List.Extra exposing (findIndex)


type SelectList a
    = SelectList (List a) a (List a)


selected : SelectList a -> a
selected (SelectList _ sel _) =
    sel


before : SelectList a -> List a
before (SelectList beforeSel _ _) =
    beforeSel


after : SelectList a -> List a
after (SelectList _ _ afterSel) =
    afterSel


cycle : SelectList a -> SelectList a
cycle selectList =
    let
        beforeList : List a
        beforeList =
            before selectList

        afterList : List a
        afterList =
            after selectList

        cur : a
        cur =
            selected selectList
    in
    case ( beforeList, afterList ) of
        ( [], [] ) ->
            selectList

        ( a :: b, c :: d ) ->
            SelectList (beforeList ++ [ cur ]) c d

        ( a :: b, [] ) ->
            SelectList [ cur ] a b

        ( [], a :: b ) ->
            SelectList [ cur ] a b


fromLists : List a -> a -> List a -> SelectList a
fromLists =
    SelectList


fromList : a -> List a -> SelectList a
fromList a domain =
    case List.Extra.findIndex ((==) a) domain of
        Nothing ->
            SelectList [] a domain

        Just n ->
            let
                xy =
                    ( List.take n domain, List.drop (n + 1) domain )
            in
            SelectList (Tuple.first xy) a (Tuple.second xy)
