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


type Form out
    = Form (List (Field out))


type Field out
    = Field
        { branch : List (Field out)
        , init : Maybe InputEvent
        , update : InputEvent -> Key -> Db out -> out
        , view : Key -> Db out -> List (Html (Msg out))
        }



-- Model


type Model out
    = Model (Db out)


type alias Key =
    List Int


type alias Db out =
    { seenInput : Bool
    , seenBlur : Bool
    , seenSubmit : Bool
    , touched : Set Key
    , touchedSinceSubmit : Set Key
    , changed : Set Key
    , changedSinceSubmit : Set Key
    , state : Dict Key String
    , output : out
    }


init : Form out -> out -> Model out
init (Form fields) emptyOut =
    let
        initField (Field field) key db =
            case field.init of
                Just event ->
                    updateDb event (Field field) key db

                Nothing ->
                    db
    in
    Db False False False Set.empty Set.empty Set.empty Set.empty Dict.empty emptyOut
        |> traverse initField fields []
        |> Model



-- Update


type Msg out
    = OnInput Key InputEvent
    | OnBlur Key
    | OnSubmit


type InputEvent
    = TargetValue String
    | TargetChecked String Bool


update : Form out -> Msg out -> Model out -> Model out
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


updateDb : InputEvent -> Field out -> Key -> Db out -> Db out
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
    (Db out -> Bool)
    -> (model -> out -> ( model, Cmd msg ))
    -> (Model out -> model)
    -> Model out
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


autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> Model out -> ( model, Cmd msg )
autoSubmit =
    onSubmit .seenInput


submitOnBlur : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> Model out -> ( model, Cmd msg )
submitOnBlur =
    onSubmit .seenBlur


onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> Model out -> ( model, Cmd msg )
onFormSubmit =
    onSubmit .seenSubmit



-- View


view : (Msg out -> msg) -> Form out -> String -> Model out -> Html msg
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


viewFields : List (Field out) -> Key -> Db out -> List (Html (Msg out))
viewFields fields prefix db =
    let
        viewField i (Field field) =
            divField
                (field.view (List.append prefix [ i ]) db)
    in
    List.indexedMap viewField fields


divField : List (Html (Msg out)) -> Html (Msg out)
divField =
    Html.div [ HA.class "field" ]



-- Creating a form


empty : Form out
empty =
    Form []


append : Field out -> Form out -> Form out
append field (Form fields) =
    Form (fields ++ [ field ])


appendIf : Bool -> Field out -> Form out -> Form out
appendIf on field form =
    if on then
        append field form

    else
        form



-- Using a form (internally)


traverse : (Field out -> Key -> Db out -> Db out) -> List (Field out) -> Key -> Db out -> Db out
traverse cb fields key db =
    List.foldl
        (\(Field field) ( i, acc2 ) ->
            let
                nextKey =
                    List.append key [ i ]
            in
            ( i + 1
            , cb (Field field) nextKey acc2
                |> traverse cb field.branch nextKey
            )
        )
        ( 0, db )
        fields
        |> Tuple.second


getField : List (Field out) -> Db out -> Key -> Field out
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
                        getField field.branch db rem
            )
        |> Maybe.withDefault emptyField



-- Creating fields


emptyField : Field out
emptyField =
    Field
        { branch = noBranch
        , init = noInit
        , update = noUpdate
        , view = \_ _ -> []
        }


noBranch : List (Field out)
noBranch =
    []


noInit : Maybe InputEvent
noInit =
    Nothing


noUpdate : InputEvent -> Key -> Db out -> out
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


valueAttrs : Key -> Db out -> List (Html.Attribute (Msg out))
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


checkedAttrs : JD.Decoder InputEvent -> Key -> String -> Db out -> List (Html.Attribute (Msg out))
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


fieldState : Key -> Model out -> FieldState
fieldState key (Model db) =
    { value = Dict.get key db.state
    , touched = Set.member key db.touched
    , touchedSinceSubmit = Set.member key db.touchedSinceSubmit
    , changed = Set.member key db.changed
    , changedSinceSubmit = Set.member key db.changedSinceSubmit
    }



-- Text fields


type alias TextConfig out =
    { common : Common String out
    , autofocus : Bool
    , inputmode : String
    , placeholder : String
    , type_ : String
    }


inputField : (String -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
inputField set attrs =
    let
        emptyTextConfig =
            TextConfig (emptyCommon "") False "" "" "text"

        c =
            attrToConfig emptyTextConfig attrs
    in
    Field
        { branch = noBranch
        , init = Just (TargetValue c.common.default)
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


type alias CheckedConfig out =
    { common : Common Bool out
    }


checkboxField : (Bool -> out -> out) -> List (Attribute (CheckedConfig out)) -> Field out
checkboxField set attrs =
    let
        c =
            attrToConfig (CheckedConfig (emptyCommon False)) attrs
    in
    Field
        { branch = noBranch
        , init = Just (TargetChecked boolValue c.common.default)
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


type alias OptionConfig option out =
    { common : Common option out
    , nothing : Maybe String
    }


emptyOptionConfig : OptionConfig (Maybe option) out
emptyOptionConfig =
    OptionConfig (emptyCommon Nothing) Nothing


type alias OptionArgs option out =
    { set : Maybe option -> out -> out
    , toString : option -> String
    , options : List option
    , attributes : List (Attribute (OptionConfig (Maybe option) out))
    }


optionInit : (option -> String) -> Maybe option -> Maybe InputEvent
optionInit toString def =
    Maybe.map (toString >> TargetValue) def


optionUpdate : OptionArgs option out -> InputEvent -> Key -> Db out -> out
optionUpdate args event _ db =
    let
        c =
            attrToConfig emptyOptionConfig args.attributes

        findOption value =
            availableOptions c.nothing args.options
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


availableOptions : Maybe String -> List option -> List (Maybe option)
availableOptions none options =
    let
        -- Wrap all options in a Maybe
        options2 =
            List.map Just options
    in
    -- If we're displaying a none option, add Nothing
    case none of
        Just _ ->
            Nothing :: options2

        Nothing ->
            options2


radioField : OptionArgs option out -> Field out
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
        , update = optionUpdate args
        , view =
            \key db ->
                withLeftElement (Html.div [])
                    c.common.label
                    (availableOptions c.nothing args.options
                        |> List.map (optionToString args.toString c.nothing >> control key db)
                        |> List.map divField
                    )
        }



-- Select field


selectField : OptionArgs option out -> Field out
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
        , update = optionUpdate args
        , view =
            \key db ->
                withLeftLabel c.common.label
                    [ availableOptions c.nothing args.options
                        |> List.map (optionToString args.toString c.nothing >> optionEl key db)
                        |> Html.select (selectAttrs key)
                    ]
        }



-- Checkboxes field


type alias CheckboxesConfig option out =
    { common : Common option out
    }


type alias CheckboxesArgs option out =
    { set : List option -> out -> out
    , toString : option -> String
    , options : List option
    , attributes : List (Attribute (CheckboxesConfig (List option) out))
    }


checkboxesField : CheckboxesArgs option out -> Field out
checkboxesField args =
    let
        c =
            attrToConfig (CheckboxesConfig (emptyCommon [])) args.attributes

        allOptions =
            args.options
                |> List.map (\selOpt -> ( args.toString selOpt, selOpt ))
                |> Dict.fromList

        innerFields =
            List.map checkbox args.options

        checkbox opt =
            let
                value =
                    args.toString opt
            in
            Field
                { branch = noBranch
                , init =
                    if List.member opt c.common.default then
                        Just (TargetChecked value True)

                    else
                        Nothing
                , update =
                    \event key db ->
                        let
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
    in
    Field
        { branch = innerFields
        , init = noInit
        , update = noUpdate
        , view =
            \key db ->
                withLeftElement (Html.div [])
                    c.common.label
                    (viewFields innerFields key db)
        }



-- Grouping (nested: Form to Field) fields


row : Form out -> Field out
row form =
    groupClassField "row" form


column : Form out -> Field out
column form =
    groupClassField "column" form


groupClassField : String -> Form out -> Field out
groupClassField groupClass (Form fields) =
    groupField (\inner -> [ Html.div [ HA.class groupClass ] inner ]) fields


groupField : (List (Html (Msg out)) -> List (Html (Msg out))) -> List (Field out) -> Field out
groupField wrapper fields =
    Field
        { branch = fields
        , init = noInit
        , update = noUpdate
        , view = \key db -> wrapper (viewFields fields key db)
        }


fieldset : String -> Form out -> Field out
fieldset title (Form fields) =
    Field
        { branch = fields
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


submit : String -> Field out
submit l =
    htmlField [ Html.button [ HA.type_ "submit" ] [ Html.text l ] ]


htmlField : List (Html (Msg out)) -> Field out
htmlField raw =
    Field
        { branch = noBranch
        , init = noInit
        , update = noUpdate
        , view = \_ _ -> raw
        }



-- Common field


type alias Common value out =
    { attrs : List (Html.Attribute (Msg out))
    , default : value
    , label : List (Html (Msg out))
    }


emptyCommon : value -> Common value out
emptyCommon emptyDef =
    Common [] emptyDef []


type alias WithCommon a value out =
    { a | common : Common value out }


withCommon : (Common v o -> Common v o) -> WithCommon a v o -> WithCommon a v o
withCommon set o =
    { o | common = set o.common }


label : List (Html (Msg out)) -> Attribute (WithCommon c value out)
label v =
    withCommon (\c -> { c | label = v })


textLabel : String -> Attribute (WithCommon c value out)
textLabel str =
    label [ Html.text str ]


htmlAttribute : Html.Attribute (Msg out) -> Attribute (WithCommon c value out)
htmlAttribute attr =
    withCommon (\c -> { c | attrs = c.attrs ++ [ attr ] })


controlId : String -> Attribute (WithCommon c value out)
controlId =
    HA.id >> htmlAttribute


class : String -> Attribute (WithCommon c value out)
class =
    HA.class >> htmlAttribute


default : value -> Attribute (WithCommon c value out)
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
