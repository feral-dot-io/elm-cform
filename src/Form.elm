module Form exposing
    ( Model, Msg, init, update, submitOnInput, submitOnChange, submitOnForm, wrapModel, view
    , Form, empty, append, appendIf
    , row, column, fieldset
    , Field, textField, intField, floatField, textareaField, checkboxField, radioField, selectField, checkboxesField, htmlField, submit
    , autoFocus, columns, inputMode, nothingOption, placeholder, rows, type_
    , class, controlId, default, htmlAttribute, label, textLabel
    )

{-| Builds a form to be used with TEA.

Forms adhere to standard TEA and are a closed abstraction. The final arg is always the `Form` to allow dynamic forms to be passed data that may be wrapped by HTTP requests. This allows default values or control options to be populated by input data.

After an update a a submit strategy can be applied to `Model` allowing you to grab the resulting output. See the `submitOn...` functions.

A form is comprised of fields which have controls that handles data.

  - A field corresponds to an output record field with a setter. It may have an associated error. Examples: a text input or multiple radio buttons.
  - Controls a field's HTML. For text inputs there is only one control but for radio fields each option will be a separate control.
  - Data is what a control will emit to the rest of the program. Data always ends up represented as a `String` in HTML.


# TEA handling

@docs Model, Msg, init, update, submitOnInput, submitOnChange, submitOnForm, wrapModel, view


# Creating a form

@docs Form, empty, append, appendIf


# Creating nested forms

@docs row, column, fieldset


# Creating fields

@docs Field, textField, intField, floatField, textareaField, checkboxField, radioField, selectField, checkboxesField, htmlField, submit


# Field attributes

Attributes can be applied to fields to decorate or change their behaviour.

@docs autoFocus, columns, inputMode, nothingOption, placeholder, rows, type_


# Common field attributes

These attributes can be applied to any field attributes.

@docs class, controlId, default, htmlAttribute, label, textLabel

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra as HE
import Json.Decode as JD
import List.Extra as List
import Set exposing (Set)


{-| A form built up from nested fields
-}
type Form out
    = Form (List (Field out))


{-| A field represents a one or more controls in the form that updates our output record.
-}
type Field out
    = Field
        { branch : List (Field out)
        , init : Maybe InputEvent
        , update : State -> Key -> InputEvent -> out -> out
        , view : State -> Key -> List (Html (Msg out))
        }



-- Model


{-| Our DB. Tracks state of our form in HTML. The database is keyed by field position so dynamically changing fields can cause problems. TODO
-}
type Model out
    = Model (Db out)


type alias Key =
    List Int


type alias State =
    Dict Key String


type alias Db out =
    { seenInput : Bool
    , seenChange : Bool
    , seenSubmit : Bool
    , visited : Set Key
    , visitedSinceSubmit : Set Key
    , changed : Set Key
    , changedSinceSubmit : Set Key
    , state : State

    -- Default values are deferred until to simplify the model (init without a form)
    , appliedInit : Bool
    , output : out
    }


{-| Creates a new form DB that relies on field default values or blank.
-}
init : out -> Model out
init emptyOut =
    Model (Db False False False Set.empty Set.empty Set.empty Set.empty Dict.empty False emptyOut)


{-| Applies field default values to output
-}
applyInit : Form out -> Db out -> Db out
applyInit (Form fields) db =
    let
        initField (Field field) key db2 =
            case field.init of
                Just event ->
                    updateDb (Form fields) key event db2

                Nothing ->
                    db2
    in
    if db.appliedInit then
        db

    else
        foldFields initField fields [] { db | appliedInit = True }



-- Update


{-| Contains a state change
-}
type Msg out
    = OnInput Key InputEvent
    | OnChange Key
    | OnInputAndChange Key InputEvent
    | OnBlur Key
    | OnSubmit


type InputEvent
    = TargetValue String
    | TargetChecked String Bool


{-| Updates our form state. To pull the resulting output, use a submit strategy via the `submitOn...` functions. Use `submitOnInput` to get all changes.
-}
update : Msg out -> Model out -> Form out -> Model out
update msg (Model db0) form =
    let
        db =
            applyInit form db0
    in
    case msg of
        OnInput key event ->
            { db | seenInput = True }
                |> updateDb form key event
                |> Model

        OnChange key ->
            Model (visitedField key { db | seenChange = True })

        OnInputAndChange key event ->
            { db | seenInput = True, seenChange = True }
                |> updateDb form key event
                |> Model

        OnBlur key ->
            Model (visitedField key db)

        OnSubmit ->
            Model { db | seenInput = True, seenChange = True, seenSubmit = True }


updateDb : Form out -> Key -> InputEvent -> Db out -> Db out
updateDb (Form fields) key event db =
    let
        (Field field) =
            getField fields db key
    in
    { db
      -- State change
        | state = updateDbState key event db.state
        , output = field.update db.state key event db.output
    }
        |> visitedField key
        |> changedField key


visitedField : Key -> Db out -> Db out
visitedField key db =
    { db
        | visited = Set.insert key db.visited
        , visitedSinceSubmit = Set.insert key db.visitedSinceSubmit
    }


changedField : Key -> Db out -> Db out
changedField key db =
    { db
        | changed = Set.insert key db.visited
        , changedSinceSubmit = Set.insert key db.visitedSinceSubmit
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
    -> model
    -> Model out
    -> ( ( model, Cmd msg ), Model out )
onSubmit seen next extModel (Model db) =
    if seen db then
        ( next extModel db.output
        , Model
            { db
                | seenInput = False
                , seenChange = False
                , seenSubmit = False
                , visitedSinceSubmit = Set.empty
                , changedSinceSubmit = Set.empty
            }
        )

    else
        ( ( extModel, Cmd.none ), Model db )


{-| Process output after on immediate (key press) form input. Effectively an auto-submit.
-}
submitOnInput : (model -> out -> ( model, Cmd msg )) -> model -> Model out -> ( ( model, Cmd msg ), Model out )
submitOnInput =
    onSubmit .seenInput


{-| Process output after a field's control has changed. For example on a checkbox click or after text written to a text field.
-}
submitOnChange : (model -> out -> ( model, Cmd msg )) -> model -> Model out -> ( ( model, Cmd msg ), Model out )
submitOnChange =
    onSubmit .seenChange


{-| Only process output after a form has been submitted. For example by using a submit button or the user pressing enter on a text field.
-}
submitOnForm : (model -> out -> ( model, Cmd msg )) -> model -> Model out -> ( ( model, Cmd msg ), Model out )
submitOnForm =
    onSubmit .seenSubmit


{-| Wraps a Form's model into an outer model. Provider a setter and send the output from an `submitOn...`
-}
wrapModel : (model -> Model out -> model) -> ( ( model, Cmd msg ), Model out ) -> ( model, Cmd msg )
wrapModel setter ( ( extModel, cmd ), ourModel ) =
    ( setter extModel ourModel
    , cmd
    )



-- View


{-| Renders a composed form. The `String` is the form ID and must be unique for the page. Forms are rendered in field append order. Applies a local stylesheet for minimal styling.
-}
view : (Msg out -> msg) -> String -> Model out -> Form out -> Html msg
view toMsg id (Model db) form =
    let
        { state } =
            applyInit form db
    in
    viewFields Html.div form [] state
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
    [ rule ".field { margin: 0.5rem 0; }"
    , rule ".row { display: flex; }"
    , rule ".row > .field { margin: 0 0.5rem; }"
    , rule ".row > .field:first-child { margin-left: 0; }"
    , rule ".column { margin: 0 0.5rem; }"
    , rule ".column > .field:first-child { margin: 0; }"
    , rule ".field > label, .field > .label { display: block; font-weight: bold; }"
    , rule ".field > input~label, .field > input~.label { display: initial; font-weight: normal; }"
    , rule "input[type=text], input[type=email], input[type=password], input[type=number], textarea, select { width: 100%; padding: 0.6rem 0.3rem; box-sizing: border-box; }"
    , rule "input[type=checkbox], input[type=radio] { margin: 0 0.5rem; }"
    , rule ".controls { list-style-type: none; margin: 0; padding: 0; align-items: stretch; }"
    , rule ".controls > li { margin: 0 0.25rem; white-space: nowrap; }"
    , rule "button { padding: 0.4rem 4rem; }"
    , rule "fieldset > .field { margin: 0; width: fit-content; }"
    ]
        |> String.join "\n"
        |> (Html.text >> List.singleton)
        |> Html.node "style" []


viewFields : (List (Html.Attribute (Msg out)) -> List (Html (Msg out)) -> Html (Msg out)) -> Form out -> Key -> State -> List (Html (Msg out))
viewFields el (Form fields) prefix state =
    let
        viewField i (Field field) =
            el [ HA.class "field" ]
                (field.view state (List.append prefix [ i ]))
    in
    List.indexedMap viewField fields



-- Creating a form


{-| Creates a form with no fields ready to be added.
-}
empty : Form out
empty =
    Form []


{-| Adds a field to a form.
-}
append : Field out -> Form out -> Form out
append field (Form fields) =
    Form (fields ++ [ field ])


{-| Conditionally adds a field to a form.
-}
appendIf : Bool -> Field out -> Form out -> Form out
appendIf on field form =
    if on then
        append field form

    else
        form



-- Using a form (internally)


foldFields : (Field out -> Key -> Db out -> Db out) -> List (Field out) -> Key -> Db out -> Db out
foldFields cb fields key db =
    List.foldl
        (\(Field field) ( i, acc2 ) ->
            let
                nextKey =
                    List.append key [ i ]
            in
            ( i + 1
            , cb (Field field) nextKey acc2
                |> foldFields cb field.branch nextKey
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


noUpdate : State -> Key -> InputEvent -> out -> out
noUpdate _ _ _ out =
    out


keyToString : Key -> String
keyToString key =
    String.join "-" (List.map String.fromInt key)


targetValueDecoder : JD.Decoder InputEvent
targetValueDecoder =
    JD.map TargetValue HE.targetValue


targetCheckedDecoder : JD.Decoder InputEvent
targetCheckedDecoder =
    JD.map2 TargetChecked HE.targetValue HE.targetChecked


valueAttrs : Key -> State -> List (Html.Attribute (Msg out))
valueAttrs key state =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( OnInput key event, True ))
            targetValueDecoder
        )
    , HE.onChange (\_ -> OnChange key)
    , HE.onBlur (OnBlur key)
    , HA.name (keyToString key)
    , HA.value (Dict.get key state |> Maybe.withDefault "")
    ]


checkedAttrs : JD.Decoder InputEvent -> Key -> String -> State -> List (Html.Attribute (Msg out))
checkedAttrs eventDecoder key value state =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( OnInputAndChange key event, True ))
            eventDecoder
        )
    , HE.onBlur (OnBlur key)
    , HA.name (keyToString key)
    , HA.value value
    , HA.checked (Dict.get key state == Just value)
    ]


selectAttrs : Key -> List (Html.Attribute (Msg out))
selectAttrs key =
    [ HE.on "change" (JD.map (OnInputAndChange key) targetValueDecoder)
    , HE.onBlur (OnBlur key)
    , HA.name (keyToString key)
    ]


withLeftLabel : List (Html (Msg out)) -> List (Html (Msg out)) -> List (Html (Msg out))
withLeftLabel left inner =
    if List.isEmpty left then
        inner

    else
        [ Html.label [] (left ++ inner) ]


withRightLabel : List (Html (Msg out)) -> List (Html (Msg out)) -> List (Html (Msg out))
withRightLabel right inner =
    if List.isEmpty right then
        inner

    else
        [ Html.label [] (inner ++ right) ]


divLabel : List (Html msg) -> List (Html msg)
divLabel l =
    if List.isEmpty l then
        []

    else
        [ Html.div [ HA.class "label" ] l ]


ifAttr : Bool -> a -> List a
ifAttr include attr =
    if include then
        [ attr ]

    else
        []


ifStrAttr : String -> a -> List a
ifStrAttr str =
    ifAttr (not (String.isEmpty str))



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
    , touched = Set.member key db.visited
    , touchedSinceSubmit = Set.member key db.visitedSinceSubmit
    , changed = Set.member key db.changed
    , changedSinceSubmit = Set.member key db.changedSinceSubmit
    }



-- Text fields


type alias TextConfig out =
    { common : Common String out
    , autoFocus : Bool
    , inputMode : String
    , placeholder : String
    , type_ : String
    }


{-| Creates a text input field.
-}
textField : (String -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
textField set attrs =
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
            \_ _ event out ->
                eventTargetValue event
                    |> Maybe.map (\v -> set v out)
                    |> Maybe.withDefault out
        , view =
            \state key ->
                withLeftLabel c.common.label
                    [ Html.input
                        (HA.type_ c.type_
                            :: ifAttr c.autoFocus (HA.autofocus c.autoFocus)
                            ++ ifStrAttr c.inputMode (HA.attribute "inputmode" c.inputMode)
                            ++ ifStrAttr c.placeholder (HA.placeholder c.placeholder)
                            ++ c.common.attrs
                            ++ valueAttrs key state
                        )
                        []
                    ]
        }


{-| A text field that tries to parse its input as an `Int`.
-}
intField : (Maybe Int -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
intField set attrs =
    textField (String.toInt >> set)
        (inputMode "numeric" :: attrs)


{-| A text field that tries to parse its input as an `Float`.
-}
floatField : (Maybe Float -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
floatField set attrs =
    textField (String.toFloat >> set)
        (inputMode "decimal" :: attrs)



-- Textarea field


type alias TextareaConfig out =
    { common : Common String out
    , autoFocus : Bool
    , columns : Int
    , placeholder : String
    , rows : Int
    }


{-| Creates a textarea field. Multi-line text input.
-}
textareaField : (String -> out -> out) -> List (Attribute (TextareaConfig out)) -> Field out
textareaField set attrs =
    let
        emptyTextConfig =
            TextareaConfig (emptyCommon "") False 0 "" 0

        c =
            attrToConfig emptyTextConfig attrs
    in
    Field
        { branch = noBranch
        , init = Just (TargetValue c.common.default)
        , update =
            \_ _ event out ->
                eventTargetValue event
                    |> Maybe.map (\v -> set v out)
                    |> Maybe.withDefault out
        , view =
            \state key ->
                withLeftLabel c.common.label
                    [ Html.textarea
                        (ifAttr c.autoFocus (HA.autofocus c.autoFocus)
                            ++ ifAttr (c.columns > 0) (HA.cols c.columns)
                            ++ ifAttr (c.rows > 0) (HA.rows c.rows)
                            ++ ifStrAttr c.placeholder (HA.placeholder c.placeholder)
                            ++ c.common.attrs
                            ++ valueAttrs key state
                        )
                        []
                    ]
        }



-- Checkbox field


type alias CheckedConfig out =
    { common : Common Bool out
    }


{-| Creates a checkbox that can be either on or off. If you have multiple values to toggle then checkboxesField may be more useful.
-}
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
            \_ _ event out ->
                eventTargetChecked event
                    |> Maybe.map Tuple.second
                    |> Maybe.map (\v -> set v out)
                    |> Maybe.withDefault out
        , view =
            \state key ->
                let
                    id =
                        keyToString key
                in
                Html.input
                    (HA.type_ "checkbox"
                        :: HA.id id
                        :: c.common.attrs
                        ++ checkedAttrs targetCheckedDecoder key boolValue state
                    )
                    []
                    :: ifAttr (not (List.isEmpty c.common.label))
                        (Html.label [ HA.id id ] c.common.label)
        }



-- Radio field


type alias OptionConfig option out =
    { common : Common option out
    , nothing : Maybe String
    }


emptyOptionConfig : OptionConfig (Maybe option) out
emptyOptionConfig =
    OptionConfig (emptyCommon Nothing) Nothing


optionInit : (option -> String) -> Maybe option -> Maybe InputEvent
optionInit toString def =
    Maybe.map (toString >> TargetValue) def


optionUpdate :
    { set : Maybe option -> out -> out
    , toString : option -> String
    , options : List option
    , attributes : List (Attribute (OptionConfig (Maybe option) out))
    }
    -> State
    -> Key
    -> InputEvent
    -> out
    -> out
optionUpdate args _ _ event out =
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
        |> Maybe.map (\v -> args.set v out)
        |> Maybe.withDefault out


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


{-| Creates a set of radio controls. The `toString` arg must produce a non-empty and unique value.
-}
radioField :
    { set : Maybe option -> out -> out
    , toString : option -> String
    , options : List option
    , attributes : List (Attribute (OptionConfig (Maybe option) out))
    }
    -> Field out
radioField args =
    let
        c =
            attrToConfig emptyOptionConfig args.attributes

        control state key value =
            withRightLabel [ Html.text value ]
                [ Html.input
                    (HA.type_ "radio"
                        :: c.common.attrs
                        ++ checkedAttrs targetValueDecoder key value state
                    )
                    []
                ]
    in
    Field
        { branch = noBranch
        , init = optionInit args.toString c.common.default
        , update = optionUpdate args
        , view =
            \state key ->
                divLabel c.common.label
                    ++ [ availableOptions c.nothing args.options
                            |> List.map (optionToString args.toString c.nothing >> control state key)
                            |> List.map (Html.li [])
                            |> Html.ul [ HA.class "controls" ]
                       ]
        }



-- Select field


{-| Creates a select field. The `toString` arg must produce a non-empty and unique value.
-}
selectField :
    { set : Maybe option -> out -> out
    , toString : option -> String
    , options : List option
    , attributes : List (Attribute (OptionConfig (Maybe option) out))
    }
    -> Field out
selectField args =
    let
        c =
            attrToConfig emptyOptionConfig args.attributes

        optionEl key state value =
            Html.option
                [ HA.value value
                , HA.selected (Dict.get key state == Just value)
                ]
                [ Html.text value ]
    in
    Field
        { branch = noBranch
        , init = optionInit args.toString c.common.default
        , update = optionUpdate args
        , view =
            \state key ->
                withLeftLabel c.common.label
                    [ availableOptions c.nothing args.options
                        |> List.map (optionToString args.toString c.nothing >> optionEl key state)
                        |> Html.select (selectAttrs key)
                    ]
        }



-- Checkboxes field


type alias CheckboxesConfig option out =
    { common : Common option out
    }


{-| Creates a set of checkboxes acting as a field. The `toString` arg must produce a non-empty and unique value. Each checkbox is an `option` that can be toggled. The `set` will receive all options that are toggled on.
-}
checkboxesField :
    { set : List option -> out -> out
    , toString : option -> String
    , options : List option
    , attributes : List (Attribute (CheckboxesConfig (List option) out))
    }
    -> Field out
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
                    \state key event out ->
                        let
                            prefix =
                                List.unconsLast key
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault []

                            activeOptions =
                                state
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
                        args.set newOptions out
                , view =
                    \db key ->
                        let
                            id =
                                keyToString key
                        in
                        [ Html.input
                            ([ HA.type_ "checkbox"
                             , HA.id id
                             ]
                                ++ c.common.attrs
                                ++ checkedAttrs targetCheckedDecoder key value db
                            )
                            []
                        , Html.label [ HA.for id ] [ Html.text value ]
                        ]
                }
    in
    Field
        { branch = innerFields
        , init = noInit
        , update = noUpdate
        , view =
            \db key ->
                divLabel c.common.label
                    ++ [ Html.ul [ HA.class "controls" ]
                            (viewFields Html.li (Form innerFields) key db)
                       ]
        }



-- Grouping (nested: Form to Field) fields


{-| Turns a nested form into a row field. Each field will appear left to right.
-}
row : Form out -> Field out
row form =
    groupClassField "row" form


{-| Turns a nested form into a column. Each field will be shown from top to bottom. Used as the default if no column or row specified.
-}
column : Form out -> Field out
column form =
    groupClassField "column" form


groupClassField : String -> Form out -> Field out
groupClassField groupClass form =
    groupField (\inner -> [ Html.div [ HA.class groupClass ] inner ]) form


groupField : (List (Html (Msg out)) -> List (Html (Msg out))) -> Form out -> Field out
groupField wrapper (Form fields) =
    Field
        { branch = fields
        , init = noInit
        , update = noUpdate
        , view = \db key -> wrapper (viewFields Html.div (Form fields) key db)
        }


{-| Wraps a form into a fieldset. Takes a title and another form.
-}
fieldset : String -> Form out -> Field out
fieldset title (Form fields) =
    Field
        { branch = fields
        , init = noInit
        , update = noUpdate
        , view =
            \db key ->
                [ Html.fieldset []
                    (Html.legend [] [ Html.text title ]
                        :: viewFields Html.div (Form fields) key db
                    )
                ]
        }



-- HTML fields


{-| Creates a submit button.
-}
submit : String -> Field out
submit l =
    htmlField [ Html.button [ HA.type_ "submit" ] [ Html.text l ] ]


{-| Creates a HTML field. It doesn't contain any controls or update the output in any way but allows you to add your own HTML in a form.
-}
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


{-| Wraps a control with a HTML based label.
-}
label : List (Html (Msg out)) -> Attribute (WithCommon c value out)
label v =
    withCommon (\c -> { c | label = v })


{-| Wraps a control with a text label.
-}
textLabel : String -> Attribute (WithCommon c value out)
textLabel str =
    label [ Html.text str ]


{-| Adds an HTML attribute to a control.
-}
htmlAttribute : Html.Attribute (Msg out) -> Attribute (WithCommon c value out)
htmlAttribute attr =
    withCommon (\c -> { c | attrs = c.attrs ++ [ attr ] })


{-| Sets the `Html.Attribute.id` of a control.
-}
controlId : String -> Attribute (WithCommon c value out)
controlId =
    HA.id >> htmlAttribute


{-| Sets the class of a control.
-}
class : String -> Attribute (WithCommon c value out)
class =
    HA.class >> htmlAttribute


{-| Sets the default value for a control. If not set the equivalent of a blank string is used.
-}
default : value -> Attribute (WithCommon c value out)
default get =
    withCommon (\c -> { c | default = get })



-- Field attributes


type alias Attribute config =
    config -> config


attrToConfig : config -> List (Attribute config) -> config
attrToConfig zero attr =
    List.foldl (\fn acc -> fn acc) zero attr


{-| Sets the `Html.Attribute.autofocus` on text and textarea fields.
-}
autoFocus : Bool -> Attribute { c | autoFocus : Bool }
autoFocus v o =
    { o | autoFocus = v }


{-| Sets the number of columns on a textarea field. Should be avoided (use CSS).
-}
columns : Int -> Attribute { c | columns : Int }
columns v o =
    { o | columns = v }


{-| Sets the `Html.Attribute.inputmode` a text input. Useful on mobile to show a relevant keyboard.
-}
inputMode : String -> Attribute { c | inputMode : String }
inputMode v o =
    { o | inputMode = v }


{-| Sets `Html.Attribute.placeholder` on a text input. Try to avoid this as it (1) vanishes on input, (2) missed by some screen readers, and (3) does not meet minimum contrast requirements. See: <https://design-system.service.gov.uk/components/text-input/#avoid-placeholder-text>
-}
placeholder : String -> Attribute { c | placeholder : String }
placeholder v o =
    { o | placeholder = v }


{-| Sets the number of rows on a textarea field.
-}
rows : Int -> Attribute { c | rows : Int }
rows v o =
    { o | rows = v }


{-| Sets `Html.Attribute.type_` on a text field. By default "text" is used. Use `checkboxField` for "checkbox" and `radioField` for "radio".
-}
type_ : String -> Attribute { c | type_ : String }
type_ v o =
    { o
        | type_ =
            -- Don't allow checked inputs
            if v == "checkbox" || v == "radio" then
                "text"

            else
                v
    }


{-| For `radioField` and `selectField` the setter function can receive a `Maybe`. This represents an item where an invalid or no option has been selected. For radio fields this means an unchecked state and select fields pick the first item if no default value is set.

The problem is that when a user clicks a radio item they can't go backwards to the previous state. This is considered bad design for forms. However if a value is pre-selected on the page then the user might submit and accidentally miss the field. So you should carefully consider what works best for your forms. Also, sometimes the `Nothing` value is a useful option.

This attribute prefixes the list of options with a nothing option that can be selected. If the no other option is selected (via default or state change) then this nothing option will be pre-selected.

-}
nothingOption : String -> Attribute { c | nothing : Maybe String }
nothingOption l o =
    { o | nothing = Just l }
