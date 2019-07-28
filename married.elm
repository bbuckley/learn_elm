module Married exposing (main)

import Browser exposing (sandbox)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, fieldset, hr, input, label, li, p, section, span, text, ul)
import Html.Attributes exposing (checked, placeholder, style, title, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Json.Decode exposing (Decoder, Error, andThen, decodeString)
import List.Extra exposing (updateIf)
import Selected exposing (cycle, fromList, selected)
import Set exposing (Set)


type alias Id =
    String


type BennyInfo
    = NoBeneficiary Ben
    | Beneficiary Ben


type Status
    = A
    | T
    | L
    | D


statusMap : List ( String, Status )
statusMap =
    [ ( "A", A )
    , ( "T", T )
    , ( "L", L )
    , ( "D", D )
    ]


type CalcType
    = UI1
    | Modeling
    | ModelingAB
    | Final
    | TermAB


calcTypeMap : List ( String, CalcType )
calcTypeMap =
    [ ( "UI1", UI1 )
    , ( "Modeling", Modeling )
    , ( "ModelingAB", ModelingAB )
    , ( "Final", Final )
    , ( "TermAB", TermAB )
    ]


type alias Ben =
    { sdob : String
    , spousal : Spousal
    , sex : Sex
    }



-- benDef : Ben
-- benDef =
--     { sdob = "", spousal = Spouse, sex = Female }


type alias Tc =
    { id : Id
    , doc : String
    , dob : String
    , doe : String
    , dot : String
    , sex : Sex
    , status : Status
    , calcType : CalcType
    , bennyInfo : BennyInfo
    , pbc : String
    , stat : Stat
    , tags : String
    }


type alias Data =
    { notes : String
    , run : String
    , certed : String
    }


type Stat
    = NeedsSettingUp Data
    | ReadyToRun Data
    | Run Data
    | Certed Data


type Spousal
    = Spouse
    | NonSpouse


type Sex
    = Male
    | Female


sexMap : List ( String, Sex )
sexMap =
    [ ( "Male", Male )
    , ( "Female", Female )
    ]


typeMapToString : List ( String, a ) -> a -> String
typeMapToString pairs key =
    pairs
        |> List.filter (\( _, v ) -> v == key)
        |> List.map (\( k, _ ) -> k)
        |> List.head
        |> Maybe.withDefault "String not supplied in list"


json : String
json =
    "Spouse"


decoderSpousal : Decoder Spousal
decoderSpousal =
    Json.Decode.string |> Json.Decode.andThen spousalFromString


spousalFromString : String -> Decoder Spousal
spousalFromString string =
    case string of
        "Spouse" ->
            Json.Decode.succeed Spouse

        "NonSpouse" ->
            Json.Decode.succeed NonSpouse

        _ ->
            Json.Decode.fail ("Invalid Spouse: " ++ string)


r : Result Error Spousal
r =
    -- Json.Decode.decodeString decoderSpousal json
    Json.Decode.decodeString decoderSpousal json


toStringModel : String
toStringModel =
    case r of
        Ok _ ->
            "Ok"

        Err _ ->
            "Error"



-- decoderSex : Json.Decode.Decoder Sex
-- decoderSex =
--     Json.Decode.string
--         |> Json.Decode.andThen
--             (\string ->
--                 case string of
--                     "Male" ->
--                         Json.Decode.succeed Male
--                     "Female" ->
--                         Json.Decode.succeed Female
--                     _ ->
--                         Json.Decode.fail ("Invalid Spouse: " ++ string)
--             )
-- list1 : List Tc
-- list1 =
--     [ { id = "1"
--       , doc = "1/9/6666"
--       , dob = "1/9/6666"
--       , doe = "1/9/6666"
--       , dot = "1/9/6666"
--       , status = A
--       , sex = Male
--       , calcType = Modeling
--       , pbc = "Brian"
--       , bennyInfo = NoBeneficiary benDef
--       , stat = NeedsSettingUp dataDef
--       , tags = ""
--       }
--     , { id = "2"
--       , doc = "1/9/6666"
--       , dob = "1/9/6666"
--       , doe = "1/9/6666"
--       , dot = "1/9/6666"
--       , status = A
--       , sex = Male
--       , calcType = Modeling
--       , pbc = "Brian"
--       , bennyInfo =
--             Beneficiary
--                 { sdob = "12/22/1959"
--                 , spousal = Spouse
--                 , sex = Female
--                 }
--       , stat = NeedsSettingUp dataDef
--       , tags = ""
--       }
--     , { id = "333"
--       , doc = "1/9/6666"
--       , dob = "1/9/6666"
--       , doe = "1/9/6666"
--       , dot = "1/9/6666"
--       , status = A
--       , sex = Male
--       , calcType = Modeling
--       , pbc = "Brian"
--       , bennyInfo = Beneficiary benDef
--       , stat = NeedsSettingUp dataDef
--       , tags = ""
--       }
--     ]


toString : Spousal -> String
toString spousal =
    case spousal of
        Spouse ->
            "Spouse"

        NonSpouse ->
            "NonSpouse"


toStringBennyInfo : BennyInfo -> String
toStringBennyInfo bennyInfo =
    case bennyInfo of
        NoBeneficiary _ ->
            "No Beneficiary"

        Beneficiary { sdob, spousal, sex } ->
            "Beneficiary" ++ " [" ++ sdob ++ ", " ++ toString spousal ++ ", " ++ typeMapToString sexMap sex ++ "]"


point : Id -> BennyInfo -> List (Attribute Msg)
point id bi =
    [ style "cursor" "pointer", onClick (FooMsg id bi) ]


type Msg
    = FooMsg Id BennyInfo
    | UpdateSpousalMsg Id
    | MouseEnter Id
    | MouseLeave
    | AddOrRemoveBenny Id
    | ChangeString String Id String
    | ChangeCalcType Id
    | ChangeStatus Id
    | Clone Id
    | Delete Id
    | ToggleFilter
    | ToggleIndividualFilter Id
    | ToggleExpanded Id
    | NewOne
    | DeleteAll
    | ChangeStat Id
    | ToggleOnDesktop Id
    | SelectTag String
    | DeselectTag String



-- | RepopulateTags


red : Html.Attribute Msg
red =
    style "color" "red"


black : Html.Attribute Msg
black =
    style "color" "black"


colortc : Tc -> Model -> Html.Attribute Msg
colortc tc model =
    case model.editing of
        Nothing ->
            black

        Just id ->
            if id == tc.id then
                red

            else
                black


pointer : List (Html.Attribute Msg)
pointer =
    [ style "cursor" "pointer" ]


pointerMsg : Html.Attribute Msg -> List (Html.Attribute Msg)
pointerMsg msg =
    [ style "cursor" "pointer", msg ]



-- pointerClickToChangeMsg : String -> Html.Attribute Msg -> List (Html.Attribute Msg)
-- pointerClickToChangeMsg tooltip msg =
--     [ style "cursor" "pointer", title tooltip, msg ]
-- lii : String -> String -> Tc -> Html Msg
-- lii field currentValue tc =
--     li [] [ label pointer [ text field, input [ value currentValue, onInput (ChangeString field tc.id) ] [] ] ]
-- liDate : String -> String -> (String -> Msg) -> Html Msg
-- liDate name datev msg =
--     li [] [ label pointer [ text name, input [ placeholder "mm/dd/yyyy", value datev, onInput msg ] [] ] ]


toHtmlTc : Html.Attribute Msg -> Tc -> Model -> Html Msg
toHtmlTc color tc model =
    if List.all (\id -> tc.id /= id) model.expanded then
        toExpandedHtmlTc color tc model

    else
        toThinHtmlTc color tc model


mark : Model -> Id -> String
mark model i =
    if List.any (\id -> id == i) model.expanded then
        String.fromChar (Char.fromCode 8854)

    else
        String.fromChar (Char.fromCode 10753)


toThinHtmlTc : Html.Attribute Msg -> Tc -> Model -> Html Msg
toThinHtmlTc color tc model =
    let
        i =
            tc.id
    in
    p [ onMouseLeave MouseLeave, onMouseEnter (MouseEnter i), style "border" "dotted", color ]
        [ span []
            [ span
                [ style "cursor" "pointer"
                , style "font-size" "24px"
                , onClick (ToggleExpanded i)
                ]
                [ text (mark model i ++ tc.id) ]
            ]
        ]


toExpandedHtmlTc : Html.Attribute Msg -> Tc -> Model -> Html Msg
toExpandedHtmlTc color tc model =
    let
        i =
            tc.id
    in
    ul [ onMouseLeave MouseLeave, onMouseEnter (MouseEnter i), style "border" "dotted", color ]
        -- [ li ((ToggleOnDesktop i |> onClick) |> pointerMsg) [ span [] [ text "x" ], "id " ++ i |> text, button [ onClick (Clone i) ] [ text "clone" ], button [ onClick (Delete i) ] [ text "delete" ], checkboxi (ToggleIndividualFilter i) "filter" i model ]
        [ li [] [ span ((ToggleExpanded i |> onClick) |> pointerMsg) [ text (mark model i) ], span ((ToggleOnDesktop i |> onClick) |> pointerMsg) [ "id " ++ i |> text ], button [ onClick (Clone i) ] [ text "clone" ], button [ onClick (Delete i) ] [ text "delete" ], checkboxi (ToggleIndividualFilter i) "filter" i model ]
        , li [] [ label pointer [ text "dob", input [ placeholder "mm/dd/yyyy", value tc.dob, onInput (ChangeString "dob" i) ] [] ] ]
        , li [] [ label pointer [ text "doe", input [ placeholder "mm/dd/yyyy", value tc.doe, onInput (ChangeString "doe" i) ] [], text (calcAge tc.dob tc.doe) ] ]
        , li [] [ label pointer [ text "dot", input [ placeholder "mm/dd/yyyy", value tc.dot, onInput (ChangeString "dot" i) ] [], text (calcAge tc.dob tc.dot) ] ]
        , li [] [ label pointer [ text "doc", input [ placeholder "mm/dd/yyyy", value tc.doc, onInput (ChangeString "doc" i) ] [], text (calcAge tc.dob tc.doc) ] ]
        , li [] [ label pointer [ text "pbc", input [ value tc.pbc, onInput (ChangeString "pbc" i) ] [] ] ]
        , li [] [ label (pointerMsg (ChangeCalcType i |> onClick)) [ text "calc type" ], typeMapToString calcTypeMap tc.calcType |> text ]
        , li [] [ label (pointerMsg (onClick (ChangeStatus i))) [ text "status" ], typeMapToString statusMap tc.status |> text ]
        , li [ title "click to change", style "cursor" "pointer", onClick (ChangeStatus i) ] [ typeMapToString statusMap tc.status |> text ]
        , toHtmlBennyInfo tc
        , liStat tc
        , li []
            [ label pointer [ text "tags", input [ placeholder "add tags", value tc.tags, onInput (ChangeString "tags" i) ] [] ]
            , tc.tags |> tags
            ]
        ]



-- uniq list =
--     list |> Set.fromList |> Set.toList


uniq : List comparable -> List comparable
uniq =
    Set.fromList >> Set.toList


freq : List Tc -> Dict Word (List Id)
freq list =
    list
        |> List.map
            (\tc ->
                ( tc.id
                , tc.tags
                    |> String.replace "," " "
                    |> String.split " "
                    |> List.map (\s -> String.trim s)
                    |> List.filter (\s -> String.length s /= 0)
                    |> uniq
                )
            )
        |> List.foldl addTags Dict.empty


addTag : ( Id, Word ) -> Dict Word (List Id) -> Dict Word (List Id)
addTag ( id, word ) d =
    if Dict.member word d then
        Dict.update word (Maybe.map (\v -> id :: v)) d

    else
        Dict.insert word [ id ] d


addTags : ( Id, List Word ) -> Dict Word (List Id) -> Dict Word (List Id)
addTags ( id, words ) d =
    List.foldl (\word -> addTag ( id, word )) d words



-- hist : List ( Id, List Word ) -> Dict Word (List Id)
-- hist list =
--     List.foldl addTags Dict.empty list


tags : String -> Html Msg
tags =
    String.replace "," " "
        >> String.split " "
        >> List.map (\s -> String.trim s)
        >> List.filter (\s -> String.length s /= 0)
        >> uniq
        >> List.length
        >> String.fromInt
        >> text


liStat : Tc -> Html Msg
liStat tc =
    case tc.stat of
        NeedsSettingUp _ ->
            li [] [ label (pointerMsg (onClick (ChangeStat tc.id))) [ text "stat" ], text "Needs Setup" ]

        ReadyToRun _ ->
            li [] [ label (pointerMsg (onClick (ChangeStat tc.id))) [ text "stat" ], text "Ready to run" ]

        Run { run } ->
            li []
                [ label (pointerMsg (onClick (ChangeStat tc.id))) [ text "stat" ]
                , text "Run"
                , input
                    [ placeholder "enter run date"
                    , value run
                    , onInput (ChangeString "run" tc.id)
                    ]
                    []
                ]

        Certed { notes, run, certed } ->
            li []
                [ (ChangeStat tc.id
                    |> onClick
                    |> pointerMsg
                    |> label
                  )
                    [ text "stat" ]
                , text "Certed"
                , input
                    [ placeholder "enter run date"
                    , value run
                    , onInput <| ChangeString "run" tc.id
                    ]
                    []
                , input
                    [ placeholder "enter certed date"
                    , value certed
                    , onInput <| ChangeString "certed" tc.id
                    ]
                    []
                , input
                    [ placeholder "enter data notes"
                    , value notes
                    , onInput <| ChangeString "notes" tc.id
                    ]
                    []
                ]


toHtmlBennyInfo : Tc -> Html Msg
toHtmlBennyInfo tc =
    case tc.bennyInfo of
        NoBeneficiary _ ->
            li (pointerMsg (AddOrRemoveBenny tc.id |> onClick)) [ text "No beneficiary" ]

        Beneficiary { sdob, spousal, sex } ->
            li []
                [ label (pointerMsg (AddOrRemoveBenny tc.id |> onClick)) [ text "Beneficiary" ]
                , ul []
                    [ li ((UpdateSpousalMsg tc.id |> onClick) |> pointerMsg) [ toString spousal |> text ]
                    , li []
                        [ label [ style "cursor" "pointer" ] [ text "sdob", input [ value sdob, onInput (ChangeString "bennyInfo.sdob" tc.id) ] [] ]
                        ]
                    , li (point tc.id tc.bennyInfo ++ [ title "click to change" ]) [ typeMapToString sexMap sex |> text ]
                    ]
                ]



-- toLi : String -> Html msg
-- toLi str =
--     li [] [ text str ]


type alias Word =
    String


type alias Words =
    { include : List Word -- in Model?
    , exclude : List Word -- in Model?
    , dict : Dict Word (List Id) -- in Model? generate dynamically?
    , tags : List Word -- dict.keys?
    , selected : Set Word
    }



-- wordsDef = Words {include=[],exclude=[],dict=Dict.empty,tags=[],selected=Set.empty}
-- wordsDef =
--     Words [] [] Dict.empty [] Set.empty


type alias Model =
    { data : List Tc

    -- , editing : { id : Maybe Id }
    , editing : Maybe Id
    , idGen : IdGenerator
    , filterOn : Bool
    , individualFilterOn : List Id
    , onDesktop : List Id
    , expanded : List Id
    , words : Words

    -- , tags : String
    }


initModel : Model
initModel =
    { data = []
    , editing = Nothing
    , idGen = initIdGen
    , filterOn = True
    , individualFilterOn = []
    , onDesktop = []
    , expanded = []
    , words = Words [] [] Dict.empty [ "aa", "bb", "cc" ] (Set.fromList [ "bb" ])
    }



-- toLis : BennyInfo -> Html Msg
-- toLis =
--     toStringBennyInfo >> (\s -> li [] [ text s ])


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view }


spouseDomain : List Spousal
spouseDomain =
    [ Spouse, NonSpouse ]


updateSpousal : BennyInfo -> BennyInfo
updateSpousal bi =
    case bi of
        NoBeneficiary b ->
            NoBeneficiary b

        Beneficiary b ->
            Beneficiary
                { b
                    | spousal = fromList b.spousal spouseDomain |> cycle |> selected
                }



-- updateDob : String -> BennyInfo -> BennyInfo
-- updateDob string bi =
--     case bi of
--         NoBeneficiary b ->
--             NoBeneficiary { b | sdob = string }
--         Beneficiary b ->
--             Beneficiary { b | sdob = string }


flipSex : BennyInfo -> BennyInfo
flipSex bi =
    case bi of
        NoBeneficiary _ ->
            bi

        Beneficiary b ->
            if b.sex == Male then
                Beneficiary { b | sex = Female }

            else
                Beneficiary { b | sex = Male }



-- reformatDate : String -> String
-- reformatDate mdy =
--     mdy
--         |> String.split "/"
--         |> (\i -> ( i, List.length i ))
--         |> (\( i, n ) ->
--                 -- [m, d, y] -> [y, m, d]
--                 List.take 1 (List.reverse i)
--                     ++ List.take (n - 1) i
--            )
--         |> String.join "-"


isDate : String -> Bool
isDate date =
    case String.split "/" date of
        [ m, d, y ] ->
            [ m, d, y ] |> List.map String.toInt |> List.all ((/=) Nothing)

        _ ->
            False


type alias Date =
    String


year : Date -> Int
year date =
    case String.split "/" date of
        [ _, _, y ] ->
            Maybe.withDefault 0
                (String.toInt y)

        _ ->
            0


calcAge : String -> String -> String
calcAge a b =
    if isDate a && isDate b then
        let
            age =
                year b - year a
        in
        if age >= 0 && age <= 130 then
            String.fromInt age

        else
            ""

    else
        ""



-- benDef =
--     { sdob = "", sex = Male, spousal = Spouse }


-- type alias IdGenerator =
--     { next : Id }

type alias IdGenerator = Id



-- type Gen Id


initIdGen : IdGenerator
initIdGen =
    "1000" 
    -- { next = "1000" }

nextId : IdGenerator -> IdGenerator
nextId idGen =
    -- { idGen
    --     | next =
            case String.toInt idGen of
                Nothing ->
                    "1000"

                Just i ->
                    String.fromInt (i + 1)
    -- }


newOne : String -> Tc
newOne id =
    { tcDef | id = id }


tcDef : Tc
tcDef =
    { id = ""
    , doc = ""
    , dob = ""
    , doe = ""
    , dot = ""
    , status = A
    , sex = Male
    , calcType = Modeling
    , pbc = ""
    , bennyInfo = Beneficiary { sdob = "", spousal = Spouse, sex = Female }
    , stat = NeedsSettingUp dataDef
    , tags = ""
    }



-- new : Id -> Tc
-- new id =
--     { id = id
--     , doc = ""
--     , dob = ""
--     , doe = ""
--     , dot = ""
--     , status = A
--     , sex = Male
--     , calcType = Modeling
--     , pbc = ""
--     , bennyInfo = NoBeneficiary { sdob = "", spousal = Spouse, sex = Female }
--     , stat = NeedsSettingUp dataDef
--     , tags = ""
--     }


calcTypeDomain : List CalcType
calcTypeDomain =
    [ UI1, Modeling, ModelingAB, Final, TermAB ]


statusDomain : List Status
statusDomain =
    [ A, T, L, D ]



-- statDomain : Data -> List Stat
-- statDomain data =
--     [ NeedsSettingUp data, ReadyToRun data, Run data, Certed data ]


updateCalcType : Id -> List Tc -> List Tc
updateCalcType i =
    updateIf
        (\a -> a.id == i)
        (\a -> { a | calcType = fromList a.calcType calcTypeDomain |> cycle |> selected })


dataDef : Data
dataDef =
    { notes = "", run = "", certed = "" }


updateStat : Id -> List Tc -> List Tc
updateStat i list =
    let
        id =
            \a -> a.id == i

        x tc =
            case tc.stat of
                NeedsSettingUp data ->
                    ReadyToRun data

                ReadyToRun data ->
                    Run data

                Run data ->
                    Certed data

                Certed data ->
                    NeedsSettingUp data

        -- statNew v =
        --     { v | stat = x }
    in
    updateIf id (\v -> { v | stat = x v }) list


updateStatus : Id -> List Tc -> List Tc
updateStatus i =
    let
        id =
            \a -> a.id == i

        statusNew v =
            { v | status = fromList v.status statusDomain |> cycle |> selected }
    in
    updateIf id statusNew


flipBenny : Tc -> Tc
flipBenny tc =
    let
        benny =
            case tc.bennyInfo of
                NoBeneficiary b ->
                    Beneficiary b

                Beneficiary b ->
                    NoBeneficiary b
    in
    { tc | bennyInfo = benny }


updateField : Id -> String -> String -> List Tc -> List Tc
updateField i field string =
    updateIf (\a -> a.id == i) (updateTc field string)


updateTc : String -> String -> Tc -> Tc
updateTc field value tc =
    case field of
        "dob" ->
            { tc | dob = value }

        "doc" ->
            { tc | doc = value }

        "dot" ->
            { tc | dot = value }

        "doe" ->
            { tc | doe = value }

        "pbc" ->
            { tc | pbc = value }

        "tags" ->
            { tc | tags = value }

        "bennyInfo.sdob" ->
            { tc
                | bennyInfo =
                    case tc.bennyInfo of
                        NoBeneficiary b ->
                            NoBeneficiary { b | sdob = value }

                        Beneficiary b ->
                            Beneficiary { b | sdob = value }
            }

        "run" ->
            case tc.stat of
                Run data ->
                    { tc | stat = Run { data | run = value } }

                Certed data ->
                    { tc | stat = Certed { data | run = value } }

                ReadyToRun data ->
                    { tc | stat = ReadyToRun { data | run = value } }

                NeedsSettingUp data ->
                    { tc | stat = NeedsSettingUp { data | run = value } }

        "certed" ->
            case tc.stat of
                Run data ->
                    { tc | stat = Run { data | certed = value } }

                Certed data ->
                    { tc | stat = Certed { data | certed = value } }

                ReadyToRun data ->
                    { tc | stat = ReadyToRun { data | certed = value } }

                NeedsSettingUp data ->
                    { tc | stat = NeedsSettingUp { data | certed = value } }

        _ ->
            tc


hasId : Id -> Tc -> Bool
hasId i tc =
    tc.id == i



-- hasI : Id -> Id -> Bool
-- hasI i id =
--     id == i


insertAfter : Id -> Tc -> List Tc -> List Tc
insertAfter i b list =
    case List.Extra.findIndex (hasId i) list of
        Just n ->
            List.take (n + 1) list ++ (b :: List.drop (n + 1) list)

        Nothing ->
            b :: list


toggle : Id -> List Id -> List Id
toggle i list =
    if List.any (\id -> id == i) list then
        List.filter (\id -> id /= i) list

    else
        i :: list


update : Msg -> Model -> Model
update msg model =
    case msg of
        DeleteAll ->
            { model | data = [], individualFilterOn = [], onDesktop = [], expanded = [] }

        NewOne ->
            let
                idGen =
                    model.idGen |> nextId
            in
            { model
                | idGen = idGen
                , data = newOne idGen :: model.data
                , expanded = idGen :: model.expanded
            }

        Clone i ->
            let
                a : Tc
                a =
                    newOne model.idGen

                mod =
                    { model | idGen = model.idGen |> nextId }
            in
            case List.Extra.findIndex (hasId i) model.data of
                Nothing ->
                    { mod
                        | data = model.data ++ [ a ]
                    }

                Just ii ->
                    { mod
                        | data = insertAfter (String.fromInt ii) a model.data
                    }

        Delete i ->
            { model
                | data = model.data |> List.filter (\tc -> tc.id /= i)
                , individualFilterOn = model.individualFilterOn |> List.filter (\id -> id /= i)
                , onDesktop = model.onDesktop |> List.filter (\id -> id /= i)
                , expanded = model.expanded |> List.filter (\id -> id /= i)
            }

        ChangeStat i ->
            { model | data = model.data |> updateStat i }

        ChangeCalcType i ->
            { model | data = model.data |> updateCalcType i }

        ChangeStatus i ->
            { model | data = model.data |> updateStatus i }

        ToggleFilter ->
            { model | filterOn = not model.filterOn }

        ToggleIndividualFilter i ->
            { model | individualFilterOn = toggle i model.individualFilterOn }

        ChangeString field i string ->
            { model | data = model.data |> updateField i field string }

        AddOrRemoveBenny i ->
            { model | data = model.data |> updateIf (\a -> a.id == i) flipBenny }

        MouseEnter i ->
            { model | editing = Just i }

        MouseLeave ->
            { model | editing = Nothing }

        ToggleOnDesktop i ->
            { model | onDesktop = toggle i model.onDesktop }

        ToggleExpanded i ->
            { model | expanded = toggle i model.expanded }

        -- ChangeTags i string ->
        --     { model | data = model.data |> updateField i "tags" string }
        SelectTag tag ->
            let
                w =
                    model.words
            in
            { model | words = { w | selected = Set.insert tag w.selected } }

        DeselectTag tag ->
            let
                w =
                    model.words
            in
            { model | words = { w | selected = Set.remove tag w.selected } }

        -- RepopulateTags ->
        --     let
        --         w =
        --             model.words
        --         -- x =
        --         --     Dict.keys (freq model.data)
        --     in
        --     { model | words = { w | tags = [], selected = Set.empty } }
        -- ChangeSdob i date ->
        --     let
        --         d =
        --             model.data
        --                 |> List.map
        --                     (\v ->
        --                         if i == v.id then
        --                             { v | bennyInfo = updateDob date v.bennyInfo }
        --                         else
        --                             v
        --                     )
        --     in
        --     { model | data = d }
        -- ChangeSdob i date ->
        --     let
        --         d =
        --             model.data
        --                 |> List.map
        --                     (\v ->
        --                         if i == v.id then
        --                             { v | bennyInfo = updateDob date v.bennyInfo }
        --                         else
        --                             v
        --                     )
        --     in
        --     { model | data = d }
        UpdateSpousalMsg i ->
            { model
                | data =
                    model.data
                        |> List.map
                            (\v ->
                                if i == v.id then
                                    { v | bennyInfo = updateSpousal v.bennyInfo }

                                else
                                    v
                            )
            }

        FooMsg id _ ->
            { model
                | data =
                    model.data
                        |> List.map
                            (\v ->
                                { v
                                    | bennyInfo =
                                        if id == v.id then
                                            flipSex v.bennyInfo

                                        else
                                            v.bennyInfo
                                }
                            )
            }


toStringTc : Tc -> String
toStringTc tc =
    tc.id
        ++ " "
        ++ " dob "
        ++ tc.dob
        ++ " doc "
        ++ tc.doc
        ++ toStringBennyInfo tc.bennyInfo


toS : Model -> String
toS model =
    let
        d =
            model.data
                |> List.map toStringTc
                |> String.concat
    in
    "editing: "
        ++ (case model.editing of
                Just i ->
                    "Just " ++ i

                Nothing ->
                    "Nothing"
           )
        ++ ", data: "
        ++ d
        ++ ", filterOn: "
        ++ (if model.filterOn then
                "true"

            else
                "false"
           )
        ++ ", individualFilter: "
        ++ String.join "," model.individualFilterOn


testBracket : String -> String -> String
testBracket lab result =
    "[" ++ lab ++ " " ++ result ++ "] "


view : Model -> Html Msg
view model =
    let
        filterTc : Tc -> Bool
        filterTc tc =
            if model.filterOn then
                List.all (\i -> i /= tc.id) model.individualFilterOn

            else
                List.all (always True) model.individualFilterOn
    in
    div []
        [ section []
            [ span []
                [ model.data
                    |> List.length
                    |> String.fromInt
                    |> testBracket "count"
                    |> text
                ]
            , span []
                [ model.data
                    |> List.filter filterTc
                    |> List.length
                    |> String.fromInt
                    |> testBracket "filtered count"
                    |> text
                ]
            , span [] [ checkboxg ToggleFilter "filter" model ]
            , span [] [ button [ onClick NewOne ] [ text "New" ] ]
            , span [] [ button [ onClick DeleteAll ] [ text "Delete all" ] ]
            ]
        , div [ style "font-family" "monospace" ]
            [ model.data
                |> List.filter (\tc -> List.any (\i -> i == tc.id) model.onDesktop)
                |> List.filter filterTc
                |> List.map (\tc -> toHtmlTc (colortc tc model) tc model)
                |> ul []
            ]
        , div [ style "font-family" "monospace" ]
            [ model.data
                |> List.filter filterTc
                |> List.map (\tc -> toHtmlTc (colortc tc model) tc model)
                |> ul []
            ]
        , hr [] []
        , div [] [ text (model |> toS) ]
        , div [] [ text toStringModel ]
        , hr [] []
        , div [] [ text (freq model.data |> Dict.keys |> String.join " ") ]
        , div []
            [ text
                (freq model.data
                    |> Dict.toList
                    |> List.map (\( k, v ) -> ( k, String.join "," v ))
                    |> List.map (\( k, v ) -> k ++ "->[" ++ v ++ "]")
                    |> String.join " - "
                )
            ]
        , div []
            [ fieldset [] (List.map (checkbox model.words.selected) model.words.tags)
            ]
        ]


checkbox : Set String -> String -> Html Msg
checkbox selectedTags tag =
    let
        isChecked =
            Set.member tag selectedTags

        msg =
            if isChecked then
                DeselectTag tag

            else
                SelectTag tag
    in
    label [ style "display" "block" ] [ input [ type_ "checkbox", checked isChecked, onClick msg ] [], text tag ]


checkboxg : msg -> String -> Model -> Html msg
checkboxg msg name model =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", checked model.filterOn, onClick msg ] []
        , text name
        ]


checkboxi : msg -> String -> Id -> Model -> Html msg
checkboxi msg name i model =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", checked (not (List.any (\id -> id == i) model.individualFilterOn)), onClick msg ] []
        , text name
        ]
