module Married exposing (Ben, BennyInfo(..), CalcType(..), Data, Date, Id, IdGenerator, Model, Msg(..), Sex(..), Spousal(..), Stat(..), Status(..), Tc, benDef, black, calcAge, calcTypeDomain, calcTypeMap, checkboxg, checkboxi, colortc, dataDef, decoderSpousal, flipBenny, flipSex, hasId, initIdGen, initModel, insertAfter, isDate, json, liDate, liStat, lii, list1, main, new, newOne, nextId, point, pointer, pointerMsg, r, red, reformatDate, sexMap, spousalFromString, spouseDomain, statDomain, statusDomain, statusMap, tc1, testBracket, toHtmlBennyInfo, toHtmlTc, toLi, toLis, toS, toString, toStringBennyInfo, toStringModel, toStringTc, typeMapToString, update, updateCalcType, updateDob, updateField, updateSpousal, updateStat, updateStatus, updateTc, view, year)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, placeholder, style, type_, value, title)
import Html.Events exposing (onClick, onDoubleClick, onInput, onMouseEnter, onMouseLeave)
import Json.Decode exposing (Decoder, Error, andThen, decodeString)
import List.Extra exposing (updateIf)
import Selected exposing (..)


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
    { sdob : String, spousal : Spousal, sex : Sex }


benDef : Ben
benDef =
    { sdob = "", spousal = Spouse, sex = Female }


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
    }


type alias Data =
    { notes : String, run : String, certed : String }


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
        |> List.filter (\( k, v ) -> v == key)
        |> List.map (\( k, v ) -> k)
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



-- decodeSpousal : Decoder Spousal -> String -> Result Error Spousal
-- decodeSpousal =
--     oneOf [decodeMale, decodeFemale]


r : Result Error Spousal
r =
    -- Json.Decode.decodeString decoderSpousal json
    Json.Decode.decodeString decoderSpousal json


toStringModel : String
toStringModel =
    case r of
        Ok _ ->
            "Ok"

        Err e ->
            "Error"



-- decodeSex : Json.Decode.Decoder Sex
-- decodeSex =
--     let
--         decodeToType string =
--             case string of
--                 "Male" -> Result.Ok Male
--                 "Female" -> Result.Ok Female
--                 _ -> Result.Err ("Not valid pattern for decoder to Sex. Pattern: " ++ string)
--     in
--     Json.Decode.decodeValue Json.Decode.string decodeToType


decoderSex : Json.Decode.Decoder Sex
decoderSex =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "Male" ->
                        Json.Decode.succeed Male

                    "Female" ->
                        Json.Decode.succeed Female

                    _ ->
                        Json.Decode.fail ("Invalid Spouse: " ++ string)
            )

-- decoderModel : Json.Decode.Decoder Model
-- decoderModel =

list1 : List Tc
list1 =
    [ { id = "1"
      , doc = "1/9/6666"
      , dob = "1/9/6666"
      , doe = "1/9/6666"
      , dot = "1/9/6666"
      , status = A
      , sex = Male
      , calcType = Modeling
      , pbc = "Brian"
      , bennyInfo = NoBeneficiary benDef
      , stat = NeedsSettingUp dataDef
      }
    , { id = "2"
      , doc = "1/9/6666"
      , dob = "1/9/6666"
      , doe = "1/9/6666"
      , dot = "1/9/6666"
      , status = A
      , sex = Male
      , calcType = Modeling
      , pbc = "Brian"
      , bennyInfo =
            Beneficiary
                { sdob = "12/22/1959"
                , spousal = Spouse
                , sex = Female
                }
      , stat = NeedsSettingUp dataDef
      }
    , { id = "333"
      , doc = "1/9/6666"
      , dob = "1/9/6666"
      , doe = "1/9/6666"
      , dot = "1/9/6666"
      , status = A
      , sex = Male
      , calcType = Modeling
      , pbc = "Brian"
      , bennyInfo = Beneficiary benDef
      , stat = NeedsSettingUp dataDef
      }
    ]


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



-- attrs : Attribute Msg -> List (Attribute Msg)
-- attrs msg =
--     [ style "cursor" "pointer", msg ]


type Msg
    = FooMsg Id BennyInfo
    | UpdateSpousalMsg Id
      -- | ChangeSdob Id String
    | MouseEnter Id
    | MouseLeave
    | AddOrRemoveBenny Id
      -- | ChangeDate String Id String
    | ChangeString String Id String
    | ChangeCalcType Id
    | ChangeStatus Id
    | Clone Id
    | Delete Id
    | ToggleFilter
    | ToggleIndividualFilter Id
    | NewOne
    | DeleteAll
    | ChangeStat Id
    | ToggleOnDesktop Id


red : Html.Attribute Msg
red =
    style "color" "red"


black : Html.Attribute Msg
black =
    style "color" "black"


colortc : Tc -> Model -> Html.Attribute Msg
colortc tc model =
    case model.editing.id of
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

pointerClickToChangeMsg : String -> Html.Attribute Msg -> List (Html.Attribute Msg)
pointerClickToChangeMsg tooltip msg =
    [ style "cursor" "pointer", title tooltip, msg ]


lii : String -> String -> Tc -> Html Msg
lii field currentValue tc =
    li [] [ label pointer [ text field, input [ value currentValue, onInput (ChangeString field tc.id) ] [] ] ]


liDate : String -> String -> (String -> Msg) -> Html Msg
liDate name datev msg =
    li [] [ label pointer [ text name, input [ placeholder "mm/dd/yyyy", value datev, onInput msg ] [] ] ]


toHtmlTc : Html.Attribute Msg -> Tc -> Model -> Html Msg
toHtmlTc color tc model =
    let
        i =
            tc.id
    in
    ul [ onMouseLeave MouseLeave, onMouseEnter (MouseEnter i), style "border" "dotted", color ]
        [ li ((ToggleOnDesktop i |> onClick) |> pointerMsg) [ "id " ++ i |> text, button [ onClick (Clone i) ] [ text "clone" ], button [ onClick (Delete i) ] [ text "delete" ], checkboxi (ToggleIndividualFilter i) "filter" i model ]
        , li [] [ label pointer [ text "dob", input [ placeholder "mm/dd/yyyy", value tc.dob, onInput (ChangeString "dob" i) ] [] ] ]
        , li [] [ label pointer [ text "doe", input [ placeholder "mm/dd/yyyy", value tc.doe, onInput (ChangeString "doe" i) ] [], text (calcAge tc.dob tc.doe) ] ]
        , li [] [ label pointer [ text "dot", input [ placeholder "mm/dd/yyyy", value tc.dot, onInput (ChangeString "dot" i) ] [], text (calcAge tc.dob tc.dot) ] ]
        , li [] [ label pointer [ text "doc", input [ placeholder "mm/dd/yyyy", value tc.doc, onInput (ChangeString "doc" i) ] [], text (calcAge tc.dob tc.doc) ] ]
        , li [] [ label pointer [ text "pbc", input [ value tc.pbc, onInput (ChangeString "pbc" i) ] [] ] ]
        , li [] [ label (pointerMsg (ChangeCalcType i |> onClick)) [ text "calc type" ], typeMapToString calcTypeMap tc.calcType |> text ]
        , li [] [ label (pointerMsg (onClick (ChangeStatus i))) [ text "status" ], typeMapToString statusMap tc.status |> text ]
        , toHtmlBennyInfo tc
        , liStat tc
        ]


liStat : Tc -> Html Msg
liStat tc =
    case tc.stat of
        NeedsSettingUp data ->
            li [] [ label (pointerMsg (onClick (ChangeStat tc.id))) [ text "stat" ], text "Needs Setup" ]

        ReadyToRun data ->
            li [] [ label (pointerMsg (onClick (ChangeStat tc.id))) [ text "stat" ], text "Ready to run" ]

        Run { notes, run, certed } ->
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
                    , li ((point tc.id tc.bennyInfo) ++ [title "click to change"]) [ typeMapToString sexMap sex |> text ]
                   -- , li (pointerClickToChangeMsg "click to change" (flipSex sex))  [ typeMapToString sexMap sex |> text ]
                    ]
                ]


toLi : String -> Html msg
toLi str =
    li [] [ text str ]


type alias Model =
    { data : List Tc
    , editing : { id : Maybe Id }
    , idGen : IdGenerator
    , filterOn : Bool
    , individualFilterOn : List Id
    , onDesktop : List Id
    }


initModel =
    { data =
        list1
    , editing = { id = Nothing }
    , idGen = initIdGen
    , filterOn = True
    , individualFilterOn = []
    , onDesktop = []
    }


toLis =
    toStringBennyInfo >> (\s -> li [] [ text s ])


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


updateDob : String -> BennyInfo -> BennyInfo
updateDob string bi =
    case bi of
        NoBeneficiary b ->
            NoBeneficiary { b | sdob = string }

        Beneficiary b ->
            Beneficiary { b | sdob = string }


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


reformatDate : String -> String
reformatDate mdy =
    mdy
        |> String.split "/"
        |> (\i -> ( i, List.length i ))
        |> (\( i, n ) ->
                -- [m, d, y] -> [y, m, d]
                List.take 1 (List.reverse i)
                    ++ List.take (n - 1) i
           )
        |> String.join "-"


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


type alias IdGenerator =
    { next : Id }


initIdGen : IdGenerator
initIdGen =
    { next = "1000" }


nextId : IdGenerator -> IdGenerator
nextId idGen =
    { idGen
        | next =
            case String.toInt idGen.next of
                Nothing ->
                    "1000"

                Just i ->
                    String.fromInt (i + 1)
    }


newOne id =
    { tc1 | id = id }


tc1 : Tc
tc1 =
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
    }


new : Id -> Tc
new id =
    { id = id
    , doc = ""
    , dob = ""
    , doe = ""
    , dot = ""
    , status = A
    , sex = Male
    , calcType = Modeling
    , pbc = ""
    , bennyInfo = NoBeneficiary { sdob = "", spousal = Spouse, sex = Female }
    , stat = NeedsSettingUp dataDef
    }


calcTypeDomain =
    [ UI1, Modeling, ModelingAB, Final, TermAB ]


statusDomain =
    [ A, T, L, D ]


statDomain data =
    [ NeedsSettingUp data, ReadyToRun data, Run data, Certed data ]


updateCalcType : Id -> List Tc -> List Tc
updateCalcType i =
    updateIf
        (\a -> a.id == i)
        (\a -> { a | calcType = fromList a.calcType calcTypeDomain |> cycle |> selected })


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

        statNew v =
            { v | stat = x }
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


insertAfter : Id -> Tc -> List Tc -> List Tc
insertAfter i b list =
    case List.Extra.findIndex (hasId i) list of
        Just n ->
            List.take (n + 1) list ++ (b :: List.drop (n + 1) list)

        Nothing ->
            [ b ] ++ list


update : Msg -> Model -> Model
update msg model =
    let
        data : List Tc
        data =
            model.data
    in
    case msg of
        DeleteAll ->
            { model | data = [], individualFilterOn = [] }

        NewOne ->
            let
                idGen =
                    model.idGen |> nextId
            in
            { model | idGen = idGen, data = new idGen.next :: data }

        Clone i ->
            let
                a : Tc
                a =
                    newOne model.idGen.next

                n : Maybe Int
                n =
                    List.Extra.findIndex (hasId i) data

                mod =
                    { model | idGen = model.idGen |> nextId }
            in
            case n of
                Nothing ->
                    { mod
                        | data = data ++ [ a ]
                    }

                Just ii ->
                    { mod
                        | data = insertAfter (String.fromInt ii) a data
                    }

        Delete i ->
            { model
                | data =
                    data
                        |> List.filter (\tc -> tc.id /= i)
                , individualFilterOn = model.individualFilterOn |> List.filter (\id -> id /= i)
            }

        ChangeStat i ->
            { model | data = data |> updateStat i }

        ChangeCalcType i ->
            { model | data = data |> updateCalcType i }

        ChangeStatus i ->
            { model | data = data |> updateStatus i }

        ToggleFilter ->
            { model | filterOn = not model.filterOn }

        ToggleIndividualFilter i ->
            let
                hasI =
                    List.length (List.filter (\id -> id == i) model.individualFilterOn) /= 0
            in
            if hasI then
                { model | individualFilterOn = List.filter (\id -> id /= i) model.individualFilterOn }

            else
                { model | individualFilterOn = i :: model.individualFilterOn }

        ChangeString field i string ->
            { model | data = data |> updateField i field string }

        AddOrRemoveBenny i ->
            { model | data = data |> updateIf (\a -> a.id == i) flipBenny }

        MouseEnter i ->
            { model | editing = { id = Just i } }

        MouseLeave ->
            { model | editing = { id = Nothing } }

        ToggleOnDesktop i ->
            let
                hasI =
                    List.length (List.filter (\id -> id == i) model.onDesktop) /= 0
            in
            if hasI then
                { model | onDesktop = List.filter (\id -> id /= i) model.onDesktop }

            else
                { model | onDesktop = i :: model.onDesktop }

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

        FooMsg id bene_info ->
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
        ++ (case model.editing.id of
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
        ]


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
