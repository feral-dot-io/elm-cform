module Form exposing
    ( Form
    , Model
    , Msg
    , append
    , appendIf
    , autoSubmit
    , autofocus
    , checkboxField
    , checkboxesField
    , class
    , column
    , controlId
    , default
    , empty
    , fieldset
    , htmlAttribute
    , htmlField
    , init
    , inputField
    , inputmode
    , label
    , nothingOption
    , onFormSubmit
    , placeholder
    , radioField
    , row
    , selectField
    , submit
    , submitOnBlur
    , textLabel
    , type_
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra as HE
import Json.Decode as JD
import List.Extra as List
import Set exposing (Set)


type Form input output
    = Form (List (Field input output))


type Field input output
    = Field
        { branch : input -> List (Field input output)
        , init : input -> Maybe InputEvent
        , update : InputEvent -> Key -> Db input output -> output
        , view : Key -> Db input output -> List (Html (Msg output))
        }



-- Model


type Model input output
    = Model (Db input output)


type alias Key =
    List Int


type alias Db input output =
    { seenInput : Bool
    , seenBlur : Bool
    , seenSubmit : Bool
    , touched : Set Key
    , touchedSinceSubmit : Set Key
    , changed : Set Key
    , changedSinceSubmit : Set Key
    , state : Dict Key String
    , input : input
    , output : output
    }


init : Form input output -> output -> input -> Model input output
init (Form fields) emptyOut input =
    let
        initField (Field field) key db =
            case field.init input of
                Just event ->
                    updateDb event (Field field) key db

                Nothing ->
                    db
    in
    Db False False False Set.empty Set.empty Set.empty Set.empty Dict.empty input emptyOut
        |> traverse initField fields []
        |> Model



-- Update


type Msg output
    = OnInput Key InputEvent
    | OnBlur Key
    | OnSubmit


type InputEvent
    = TargetValue String
    | TargetChecked String Bool


update : Form input output -> Msg output -> Model input output -> Model input output
update (Form fields) msg (Model db) =
    case msg of
        OnInput key event ->
            let
                field =
                    getField fields db key
            in
            Model (updateDb event field key { db | seenInput = True })

        OnBlur _ ->
            Model { db | seenBlur = True }

        OnSubmit ->
            Model { db | seenInput = True, seenSubmit = True }


updateDb : InputEvent -> Field input output -> Key -> Db input output -> Db input output
updateDb event (Field field) key db =
    { db
      -- Touched / changed
        | touched = Set.insert key db.touched
        , touchedSinceSubmit = Set.insert key db.touchedSinceSubmit
        , changed = Set.insert key db.changed
        , changedSinceSubmit = Set.insert key db.changedSinceSubmit

        -- State change
        , state = updateDbState key event db.state
        , output = field.update event key db
    }


updateDbState : Key -> InputEvent -> Dict Key String -> Dict Key String
updateDbState key event state =
    case event of
        TargetValue value ->
            Dict.insert key value state

        TargetChecked value True ->
            Dict.insert key value state

        TargetChecked _ False ->
            Dict.remove key state


eventTargetValue : InputEvent -> Maybe String
eventTargetValue ev =
    case ev of
        TargetValue value ->
            Just value

        _ ->
            Nothing


eventTargetChecked : InputEvent -> Maybe ( String, Bool )
eventTargetChecked ev =
    case ev of
        TargetChecked value checked ->
            Just ( value, checked )

        _ ->
            Nothing


boolValue : String
boolValue =
    "y"



-- Submission handling


onSubmit :
    (Db input output -> Bool)
    -> (model -> output -> ( model, Cmd msg ))
    -> (Model input output -> model)
    -> Model input output
    -> ( model, Cmd msg )
onSubmit seen next setter (Model db) =
    let
        resetDb =
            { db
                | seenInput = False
                , seenBlur = False
                , seenSubmit = False
                , touchedSinceSubmit = Set.empty
                , changedSinceSubmit = Set.empty
            }
    in
    if seen db then
        next
            (setter (Model resetDb))
            db.output

    else
        ( setter (Model db), Cmd.none )


autoSubmit : (model -> output -> ( model, Cmd msg )) -> (Model input output -> model) -> Model input output -> ( model, Cmd msg )
autoSubmit =
    onSubmit .seenInput


submitOnBlur : (model -> output -> ( model, Cmd msg )) -> (Model input output -> model) -> Model input output -> ( model, Cmd msg )
submitOnBlur =
    onSubmit .seenBlur


onFormSubmit : (model -> output -> ( model, Cmd msg )) -> (Model input output -> model) -> Model input output -> ( model, Cmd msg )
onFormSubmit =
    onSubmit .seenSubmit



-- View


view : (Msg output -> msg) -> Form input output -> String -> Model input output -> Html msg
view toMsg (Form fields) id (Model db) =
    viewFields fields [] db
        |> (::) (stylesheet id)
        |> Html.form
            [ HA.id id
            , HE.onSubmit OnSubmit
            ]
        |> Html.map toMsg


stylesheet : String -> Html msg
stylesheet formId =
    let
        rule str =
            "#" ++ formId ++ " " ++ str
    in
    [ rule ".column { display: flex; margin: 0; }"
    , rule ".row { margin: 0 0.5rem; }"
    ]
        |> String.join "\n"
        |> (Html.text >> List.singleton)
        |> Html.node "style" []


viewFields : List (Field input output) -> Key -> Db input output -> List (Html (Msg output))
viewFields fields prefix db =
    let
        viewField i (Field field) =
            fieldDiv
                (field.view (List.append prefix [ i ]) db)
    in
    List.indexedMap viewField fields


fieldDiv : List (Html (Msg output)) -> Html (Msg output)
fieldDiv =
    Html.div [ HA.class "field" ]



-- Creating a form


empty : Form input output
empty =
    Form []


append : Field input output -> Form input output -> Form input output
append field (Form fields) =
    Form (fields ++ [ field ])


appendIf : Bool -> Field input output -> Form input output -> Form input output
appendIf on field form =
    if on then
        append field form

    else
        form



-- Using a form (internally)


traverse :
    (Field input output -> Key -> Db input output -> Db input output)
    -> List (Field input output)
    -> Key
    -> Db input output
    -> Db input output
traverse cb fields key db =
    List.foldl
        (\(Field field) ( i, acc2 ) ->
            let
                nextKey =
                    List.append key [ i ]
            in
            ( i + 1
            , cb (Field field) nextKey acc2
                |> traverse cb (field.branch db.input) nextKey
            )
        )
        ( 0, db )
        fields
        |> Tuple.second


getField : List (Field input output) -> Db input output -> Key -> Field input output
getField fields db key =
    List.head key
        |> Maybe.andThen (\i -> List.getAt i fields)
        |> Maybe.map
            (\(Field field) ->
                case List.tail key |> Maybe.withDefault [] of
                    -- End of our key, we've found our field
                    [] ->
                        Field field

                    -- Recurse deeper
                    rem ->
                        getField (field.branch db.input) db rem
            )
        |> Maybe.withDefault emptyField



-- Creating fields


emptyField : Field input output
emptyField =
    Field
        { branch = noBranch
        , init = noInit
        , update = noUpdate
        , view = \_ _ -> []
        }


noBranch : input -> List (Field input output)
noBranch _ =
    []


noInit : input -> Maybe InputEvent
noInit _ =
    Nothing


noUpdate : InputEvent -> Key -> Db input output -> output
noUpdate _ _ db =
    db.output


keyToString : Key -> String
keyToString key =
    String.join "-" (List.map String.fromInt key)


targetValueDecoder : JD.Decoder InputEvent
targetValueDecoder =
    JD.map TargetValue HE.targetValue


targetCheckedDecoder : JD.Decoder InputEvent
targetCheckedDecoder =
    JD.map2 TargetChecked HE.targetValue HE.targetChecked


valueAttrs : Key -> Db input output -> List (Html.Attribute (Msg output))
valueAttrs key db =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( OnInput key event, True ))
            targetValueDecoder
        )
    , HE.onBlur (OnBlur key)
    , HA.name (keyToString key)
    , HA.value (Dict.get key db.state |> Maybe.withDefault "")
    ]


checkedAttrs : JD.Decoder InputEvent -> Key -> String -> Db input output -> List (Html.Attribute (Msg output))
checkedAttrs eventDecoder key value db =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( OnInput key event, True ))
            eventDecoder
        )
    , HE.onBlur (OnBlur key)
    , HA.name (keyToString key)
    , HA.value value
    , HA.checked (Dict.get key db.state == Just value)
    ]


withLeftLabel : List (Html (Msg out)) -> List (Html (Msg out)) -> List (Html (Msg out))
withLeftLabel =
    withLeftElement (Html.label [])


withLeftElement : (List (Html (Msg out)) -> Html (Msg out)) -> List (Html (Msg out)) -> List (Html (Msg out)) -> List (Html (Msg out))
withLeftElement el left inner =
    if List.isEmpty left then
        inner

    else
        [ el (left ++ inner) ]


withRightLabel : List (Html (Msg out)) -> List (Html (Msg out)) -> List (Html (Msg out))
withRightLabel right inner =
    if List.isEmpty right then
        inner

    else
        [ Html.label [] (inner ++ right) ]



-- Field state


type alias FieldState =
    { value : Maybe String
    , touched : Bool
    , touchedSinceSubmit : Bool
    , changed : Bool
    , changedSinceSubmit : Bool
    }


fieldState : Key -> Model input output -> FieldState
fieldState key (Model db) =
    { value = Dict.get key db.state
    , touched = Set.member key db.touched
    , touchedSinceSubmit = Set.member key db.touchedSinceSubmit
    , changed = Set.member key db.changed
    , changedSinceSubmit = Set.member key db.changedSinceSubmit
    }



-- Text fields


type alias TextConfig input output =
    { common : Common input String output
    , autofocus : Bool
    , inputmode : String
    , placeholder : String
    , type_ : String
    }


inputField : (String -> output -> output) -> List (Attribute (TextConfig input output)) -> Field input output
inputField set attrs =
    let
        emptyTextConfig =
            TextConfig (emptyCommon "") False "" "" "text"

        c =
            attrToConfig emptyTextConfig attrs
    in
    Field
        { branch = noBranch
        , init = c.common.default >> TargetValue >> Just
        , update =
            \event _ db ->
                eventTargetValue event
                    |> Maybe.map (\v -> set v db.output)
                    |> Maybe.withDefault db.output
        , view =
            \key db ->
                withLeftLabel c.common.label
                    [ Html.input
                        (HA.type_ c.type_
                            :: c.common.attrs
                            ++ valueAttrs key db
                        )
                        []
                    ]
        }



-- Checkbox field


type alias CheckedConfig input output =
    { common : Common input Bool output
    }


checkboxField : (Bool -> output -> output) -> List (Attribute (CheckedConfig input output)) -> Field input output
checkboxField set attrs =
    let
        c =
            attrToConfig (CheckedConfig (emptyCommon False)) attrs
    in
    Field
        { branch = noBranch
        , init = c.common.default >> TargetChecked boolValue >> Just
        , update =
            \event _ db ->
                eventTargetChecked event
                    |> Maybe.map Tuple.second
                    |> Maybe.map (\v -> set v db.output)
                    |> Maybe.withDefault db.output
        , view =
            \key db ->
                withRightLabel c.common.label
                    [ Html.input
                        (HA.type_ "checkbox"
                            :: c.common.attrs
                            ++ checkedAttrs targetCheckedDecoder key boolValue db
                        )
                        []
                    ]
        }



-- Radio field


type alias OptionConfig input option output =
    { common : Common input option output
    , nothing : Maybe String
    }


emptyOptionConfig : OptionConfig input (Maybe option) output
emptyOptionConfig =
    OptionConfig (emptyCommon Nothing) Nothing


type alias OptionArgs input option output =
    { set : Maybe option -> output -> output
    , toString : option -> String
    , options : input -> List option
    , attributes : List (Attribute (OptionConfig input (Maybe option) output))
    }


optionInit : (option -> String) -> (value -> Maybe option) -> value -> Maybe InputEvent
optionInit toString def input =
    def input |> Maybe.map (toString >> TargetValue)


optionUpdate :
    OptionArgs input option output
    -> OptionConfig input (Maybe option) output
    -> InputEvent
    -> Key
    -> Db input output
    -> output
optionUpdate args c event _ db =
    let
        findOption value =
            availableOptions args.options c db
                |> List.map (\opt -> ( optionToString args.toString c.nothing opt, opt ))
                |> List.find (\( check, _ ) -> check == value)
                |> Maybe.map Tuple.second
    in
    eventTargetValue event
        |> Maybe.andThen findOption
        |> Maybe.map (\v -> args.set v db.output)
        |> Maybe.withDefault db.output


optionToString : (option -> String) -> Maybe String -> Maybe option -> String
optionToString toString none m =
    Maybe.map toString m
        |> Maybe.withDefault (Maybe.withDefault "" none)


availableOptions : (input -> List option) -> OptionConfig input (Maybe option) output -> Db input output -> List (Maybe option)
availableOptions get c db =
    let
        -- Wrap all options in a Maybe
        opts =
            List.map Just (get db.input)
    in
    -- If we're displaying a none option, add Nothing
    case c.nothing of
        Just _ ->
            Nothing :: opts

        Nothing ->
            opts


radioField : OptionArgs input option output -> Field input output
radioField args =
    let
        c =
            attrToConfig emptyOptionConfig args.attributes

        control key db value =
            withRightLabel [ Html.text value ]
                [ Html.input
                    (HA.type_ "radio"
                        :: c.common.attrs
                        ++ checkedAttrs targetValueDecoder key value db
                    )
                    []
                ]
    in
    Field
        { branch = noBranch
        , init = optionInit args.toString c.common.default
        , update = optionUpdate args c
        , view =
            \key db ->
                withLeftElement (Html.div [])
                    c.common.label
                    (availableOptions args.options c db
                        |> List.map (optionToString args.toString c.nothing >> control key db)
                        |> List.map fieldDiv
                    )
        }



-- Select field


selectField : OptionArgs input option output -> Field input output
selectField args =
    let
        c =
            attrToConfig emptyOptionConfig args.attributes

        selectAttrs key =
            [ HE.on "change" (JD.map (OnInput key) targetValueDecoder)
            , HE.onBlur (OnBlur key)
            , HA.name (keyToString key)
            ]

        optionEl key db value =
            Html.option
                [ HA.value value
                , HA.selected (Dict.get key db.state == Just value)
                ]
                [ Html.text value ]
    in
    Field
        { branch = noBranch
        , init = optionInit args.toString c.common.default
        , update = optionUpdate args c
        , view =
            \key db ->
                withLeftLabel c.common.label
                    [ availableOptions args.options c db
                        |> List.map (optionToString args.toString c.nothing >> optionEl key db)
                        |> Html.select (selectAttrs key)
                    ]
        }



-- Checkboxes field


type alias CheckboxesConfig input option output =
    { common : Common input option output
    }


type alias CheckboxesArgs input option output =
    { set : List option -> output -> output
    , toString : option -> String
    , options : input -> List option
    , attributes : List (Attribute (CheckboxesConfig input (List option) output))
    }


checkboxesField : CheckboxesArgs input option output -> Field input output
checkboxesField args =
    let
        c =
            attrToConfig (CheckboxesConfig (emptyCommon [])) args.attributes

        innerFields input =
            List.map (optionCheckbox args) (args.options input)
    in
    Field
        { branch = innerFields
        , init = noInit
        , update = noUpdate
        , view =
            \key db ->
                withLeftLabel c.common.label
                    (viewFields (innerFields db.input) key db)
        }


optionCheckbox : CheckboxesArgs input option output -> option -> Field input output
optionCheckbox args opt =
    let
        c =
            attrToConfig (CheckboxesConfig (emptyCommon [])) args.attributes

        value =
            args.toString opt
    in
    Field
        { branch = noBranch
        , init =
            \input ->
                if List.member opt (c.common.default input) then
                    Just (TargetChecked value True)

                else
                    Nothing
        , update =
            \event key db ->
                let
                    allOptions =
                        args.options db.input
                            |> List.map (\selOpt -> ( args.toString selOpt, selOpt ))
                            |> Dict.fromList

                    prefix =
                        List.unconsLast key
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault []

                    activeOptions =
                        db.state
                            |> Dict.filter (\dbKey _ -> List.isPrefixOf prefix dbKey)
                            |> Dict.values
                            |> List.filterMap (\id -> Dict.get id allOptions)

                    newOptions =
                        case eventTargetChecked event of
                            Just ( _, True ) ->
                                opt :: activeOptions

                            _ ->
                                List.filter ((/=) opt) activeOptions
                in
                args.set newOptions db.output
        , view =
            \key db ->
                withRightLabel [ Html.text value ]
                    [ Html.input
                        (HA.type_ "checkbox"
                            :: c.common.attrs
                            ++ checkedAttrs targetCheckedDecoder key value db
                        )
                        []
                    ]
        }



-- Grouping (nested: Form to Field) fields


row : Form input output -> Field input output
row form =
    groupClassField "row" form


column : Form input output -> Field input output
column form =
    groupClassField "column" form


groupClassField : String -> Form input output -> Field input output
groupClassField groupClass (Form fields) =
    groupField (\inner -> [ Html.div [ HA.class groupClass ] inner ]) fields


groupField : (List (Html (Msg output)) -> List (Html (Msg output))) -> List (Field input output) -> Field input output
groupField wrapper fields =
    Field
        { branch = always fields
        , init = noInit
        , update = noUpdate
        , view = \key db -> wrapper (viewFields fields key db)
        }


fieldset : String -> Form input output -> Field input output
fieldset title (Form fields) =
    Field
        { branch = always fields
        , init = noInit
        , update = noUpdate
        , view =
            \key db ->
                [ Html.fieldset []
                    (Html.legend [] [ Html.text title ]
                        :: viewFields fields key db
                    )
                ]
        }



-- HTML fields


submit : String -> Field input output
submit l =
    htmlField [ Html.button [ HA.type_ "submit" ] [ Html.text l ] ]


htmlField : List (Html (Msg output)) -> Field input output
htmlField raw =
    Field
        { branch = noBranch
        , init = noInit
        , update = noUpdate
        , view = \_ _ -> raw
        }



-- Common field


type alias Common input value output =
    { attrs : List (Html.Attribute (Msg output))
    , default : input -> value
    , label : List (Html (Msg output))
    }


emptyCommon : value -> Common input value output
emptyCommon emptyDef =
    Common [] (\_ -> emptyDef) []


type alias WithCommon a input value output =
    { a | common : Common input value output }


withCommon : (Common i v o -> Common i v o) -> WithCommon a i v o -> WithCommon a i v o
withCommon set o =
    { o | common = set o.common }


label : List (Html (Msg output)) -> Attribute (WithCommon c input value output)
label v =
    withCommon (\c -> { c | label = v })


textLabel : String -> Attribute (WithCommon c input value output)
textLabel str =
    label [ Html.text str ]


htmlAttribute : Html.Attribute (Msg output) -> Attribute (WithCommon c input value output)
htmlAttribute attr =
    withCommon (\c -> { c | attrs = c.attrs ++ [ attr ] })


controlId : String -> Attribute (WithCommon c input value output)
controlId =
    HA.id >> htmlAttribute


class : String -> Attribute (WithCommon c input value output)
class =
    HA.class >> htmlAttribute


default : (input -> value) -> Attribute (WithCommon c input value output)
default get =
    withCommon (\c -> { c | default = get })



-- Field attributes


type alias Attribute config =
    config -> config


attrToConfig : config -> List (Attribute config) -> config
attrToConfig zero attr =
    List.foldl (\fn acc -> fn acc) zero attr


autofocus : Bool -> Attribute { c | autofocus : Bool }
autofocus v o =
    { o | autofocus = v }


inputmode : String -> Attribute { c | inputmode : String }
inputmode v o =
    { o | inputmode = v }


placeholder : String -> Attribute { c | placeholder : String }
placeholder v o =
    { o | placeholder = v }


type_ : String -> Attribute { c | type_ : String }
type_ v o =
    { o | type_ = v }


nothingOption : String -> Attribute { c | nothing : Maybe String }
nothingOption v o =
    { o | nothing = Just v }
