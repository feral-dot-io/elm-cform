module Composable.Form exposing
    ( Attribute
    , Field
    , Form
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
    , default
    , empty
    , fieldset
    , htmlAttribute
    , htmlField
    , id
    , init
    , inputField
    , inputmode
    , label
    , maybeSelectField
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

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Form as Base
import List.Extra as List


type Form out
    = Form (List (Field out))


type alias BaseForm out =
    Base.Form Key out


type alias Key =
    Array Int


type Field out
    = Field
        { branch : Form out
        , init : BaseForm out -> Key -> Model out -> Model out
        , update : Key -> Base.Field out
        , view : ViewConfig out -> Key -> List (Html (Msg out))
        }


type alias ViewConfig out =
    { id : String
    , form : Form out
    , base : BaseForm out
    , model : Model out
    }



-- TEA


type alias Model out =
    Base.Model Key out


init : out -> Form out -> Model out
init emptyOut form =
    let
        base =
            formToBase form
    in
    traverse (\(Field field) -> field.init base)
        form
        Array.empty
        (Base.init emptyOut)


type alias Msg out =
    Base.Msg Key out


update : Msg out -> Model out -> Form out -> Model out
update msg model form =
    Base.update (formToBase form) msg model


formToBase : Form out -> BaseForm out
formToBase form key =
    findField (\(Field field) -> field.update key) form key
        |> Maybe.withDefault Base.emptyField


traverse : (Field out -> Key -> acc -> acc) -> Form out -> Key -> acc -> acc
traverse cb (Form fields) key acc =
    List.foldl
        (\(Field field) ( i, acc2 ) ->
            let
                nextKey =
                    Array.push i key
            in
            ( i + 1
            , cb (Field field) nextKey acc2
                |> traverse cb field.branch nextKey
            )
        )
        ( 0, acc )
        fields
        |> Tuple.second


findField : (Field out -> a) -> Form out -> Key -> Maybe a
findField cb (Form fields) key =
    let
        rem =
            Array.slice 1 (Array.length key) key
    in
    Array.get 0 key
        |> Maybe.andThen (\i -> List.getAt i fields)
        |> Maybe.andThen
            (\(Field field) ->
                if Array.isEmpty rem then
                    Just (cb (Field field))

                else
                    findField cb field.branch rem
            )


autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> Model out -> ( model, Cmd msg )
autoSubmit =
    Base.autoSubmit


submitOnBlur : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> Model out -> ( model, Cmd msg )
submitOnBlur =
    Base.submitOnBlur


onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> Model out -> ( model, Cmd msg )
onFormSubmit =
    Base.onFormSubmit



-- View


view : (Msg out -> msg) -> String -> Model out -> Form out -> Html msg
view toMsg formId model form =
    let
        config =
            { id = formId
            , form = form
            , base = formToBase form
            , model = model
            }

        children =
            viewForm form config Array.empty
    in
    Html.form
        (HA.id formId :: Base.formAttrs identity)
        (stylesheet formId :: children)
        |> Html.map toMsg


viewForm : Form out -> ViewConfig out -> Key -> List (Html (Msg out))
viewForm (Form fields) config prefix =
    let
        viewField i (Field field) =
            Html.div [ HA.class "field" ]
                (field.view config (Array.push i prefix))
    in
    List.indexedMap viewField fields


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



-- Building a form


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



-- Helpers


noInit : BaseForm out -> Key -> Model out -> Model out
noInit _ _ model =
    model


noUpdate : Key -> Base.Field out
noUpdate _ =
    Base.emptyField


keyToString : Key -> String
keyToString key =
    Array.map String.fromInt key
        |> Array.toList
        |> String.join "-"


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



-- Common field


type alias Common value out =
    { controlAttrs : List (Html.Attribute (Msg out))
    , default : value
    , id : String
    , label : List (Html (Msg out))
    }


emptyCommon : value -> Common value out
emptyCommon def =
    Common [] def "" []



-- Text field


type alias TextConfig out =
    { common : Common String out
    , autofocus : Bool
    , inputmode : String
    , placeholder : String
    , type_ : String
    }


emptyTextConfig : TextConfig out
emptyTextConfig =
    TextConfig (emptyCommon "") False "" "" "text"


inputField : (String -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
inputField set attrs =
    let
        c =
            attrToConfig emptyTextConfig attrs
    in
    Field
        { branch = empty
        , init = \form field -> Base.setString form field c.common.default
        , update = keyToString >> Base.stringField set
        , view =
            \{ base, model } key ->
                withLeftLabel c.common.label
                    [ Html.input
                        (HA.type_ c.type_
                            :: c.common.controlAttrs
                            ++ Base.attrs identity base key model
                        )
                        []
                    ]
        }



-- Checkbox field


type alias CheckedConfig out =
    { common : Common Bool out
    }


emptyCheckedConfig : CheckedConfig out
emptyCheckedConfig =
    CheckedConfig (emptyCommon False)


checkboxField : (Bool -> out -> out) -> List (Attribute (CheckedConfig out)) -> Field out
checkboxField set attrs =
    let
        value =
            "y"

        c =
            attrToConfig emptyCheckedConfig attrs
    in
    Field
        { branch = empty
        , init =
            \form field ->
                Base.setValue
                    form
                    field
                    (if c.common.default then
                        Just value

                     else
                        Nothing
                    )
        , update = keyToString >> Base.boolField set
        , view =
            \{ base, model } key ->
                withRightLabel c.common.label
                    [ Html.input
                        (HA.type_ "checkbox"
                            :: c.common.controlAttrs
                            ++ Base.checkedAttrs identity base key value model
                        )
                        []
                    ]
        }



-- Radios field


type alias OptionConfig option out =
    { common : Common option out
    }


emptyOptionConfig : option -> OptionConfig option out
emptyOptionConfig def =
    OptionConfig (emptyCommon def)


radioField : (Maybe option -> out -> out) -> (option -> String) -> List option -> List (Attribute (OptionConfig (Maybe option) out)) -> Field out
radioField set toString options attrs =
    let
        c =
            attrToConfig (emptyOptionConfig Nothing) attrs

        radioUpdate opt key =
            Base.checkedField
                (set (Just opt))
                (set Nothing)
                (keyToString (Array.slice 0 -1 key))

        radio opt =
            let
                value =
                    toString opt
            in
            Field
                { branch = empty
                , init =
                    \form field ->
                        Base.setValue form field (Maybe.map toString c.common.default)
                , update = radioUpdate opt
                , view =
                    \{ base, model } key ->
                        withRightLabel [ Html.text value ]
                            [ Html.input
                                (HA.type_ "radio"
                                    :: c.common.controlAttrs
                                    ++ Base.checkedAttrs identity base key value model
                                )
                                []
                            ]
                }
    in
    options
        |> List.foldl (radio >> append) empty
        |> groupField (withLeftLabel c.common.label)



-- Checkboxes


checkboxesField : (option -> out -> out) -> (option -> out -> out) -> (option -> String) -> List option -> List (Attribute (OptionConfig (List option) out)) -> Field out
checkboxesField insert remove toString options attrs =
    let
        c =
            attrToConfig (emptyOptionConfig []) attrs

        checkbox opt =
            let
                value =
                    toString opt
            in
            Field
                { branch = empty
                , init =
                    \form field model ->
                        if List.member opt c.common.default then
                            Base.setString form field value model

                        else
                            model
                , update = keyToString >> Base.checkedField (insert opt) (remove opt)
                , view =
                    \{ base, model } key ->
                        withRightLabel [ Html.text value ]
                            [ Html.input
                                (HA.type_ "checkbox"
                                    :: c.common.controlAttrs
                                    ++ Base.checkedAttrs identity base key value model
                                )
                                []
                            ]
                }
    in
    options
        |> List.foldl (checkbox >> append) empty
        |> groupField (withLeftLabel c.common.label)



-- Select field


selectField : (Maybe option -> out -> out) -> (option -> String) -> (String -> Maybe option) -> List option -> List (Attribute (OptionConfig (Maybe option) out)) -> Field out
selectField set toString fromString options attrs =
    selectField_ set toString fromString Nothing options attrs


maybeSelectField : (Maybe option -> out -> out) -> (option -> String) -> (String -> Maybe option) -> String -> List option -> List (Attribute (OptionConfig (Maybe option) out)) -> Field out
maybeSelectField set toString fromString none options attrs =
    selectField_ set toString fromString (Just none) options attrs


selectField_ : (Maybe option -> out -> out) -> (option -> String) -> (String -> Maybe option) -> Maybe String -> List option -> List (Attribute (OptionConfig (Maybe option) out)) -> Field out
selectField_ set toString fromString none options attrs =
    let
        c =
            attrToConfig (emptyOptionConfig Nothing) attrs
    in
    Field
        { branch = empty
        , init = \form field -> Base.setValue form field (Maybe.map toString c.common.default)
        , update = keyToString >> Base.stringField (fromString >> set)
        , view =
            \{ base, model } key ->
                let
                    option value inner =
                        Base.option
                            { form = base
                            , field = key
                            , value = value
                            , label = inner
                            }
                            model

                    htmlOptions =
                        List.map toString options
                            |> List.map (\value -> option value value)
                in
                withLeftLabel c.common.label
                    [ Maybe.map (option "") none
                        |> Maybe.map (\opt -> opt :: htmlOptions)
                        |> Maybe.withDefault htmlOptions
                        |> Html.select (Base.selectAttrs identity base key)
                    ]
        }



-- HTML fields


submit : String -> Field out
submit l =
    htmlField [ Html.button [ HA.type_ "submit" ] [ Html.text l ] ]


htmlField : List (Html (Msg out)) -> Field out
htmlField raw =
    Field
        { branch = empty
        , init = noInit
        , update = noUpdate
        , view = \_ _ -> raw
        }



-- Grouping (nested: Form to Field) fields


row : Form out -> Field out
row form =
    groupClassField "row" form


column : Form out -> Field out
column form =
    groupClassField "column" form


groupClassField : String -> Form out -> Field out
groupClassField groupClass =
    groupField (\inner -> [ Html.div [ HA.class groupClass ] inner ])


groupField : (List (Html (Msg out)) -> List (Html (Msg out))) -> Form out -> Field out
groupField wrapper form =
    Field
        { branch = form
        , init = noInit
        , update = noUpdate
        , view = \config key -> wrapper (viewForm form config key)
        }


fieldset : String -> Form data -> Field data
fieldset title form =
    Field
        { branch = form
        , init = noInit
        , update = noUpdate
        , view =
            \config key ->
                [ Html.fieldset []
                    (Html.legend [] [ Html.text title ]
                        :: viewForm form config key
                    )
                ]
        }



-- Common field attributes


type alias WithCommon a value out =
    { a | common : Common value out }


withCommon : (Common value out -> Common value out) -> WithCommon a value out -> WithCommon a value out
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
    withCommon (\c -> { c | controlAttrs = c.controlAttrs ++ [ attr ] })


id : String -> Attribute (WithCommon c value out)
id =
    HA.id >> htmlAttribute


class : String -> Attribute (WithCommon c value out)
class =
    HA.class >> htmlAttribute


default : value -> Attribute (WithCommon c value out)
default value =
    withCommon (\c -> { c | default = value })



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
