module Composable.Form exposing
    ( Attribute
    , Field
    , Form
    , Model
    , Msg
    , SubmitStrategy
    , append
    , appendIf
    , autoSubmit
    , autofocus
    , checkboxField
    , checkboxesField
    , class
    , column
    , empty
    , fieldset
    , htmlAttribute
    , id
    , init
    , inputField
    , inputmode
    , label
    , onFormSubmit
    , placeholder
    , radioField
    , row
    , selectField
    , submit
    , textLabel
    , type_
    , update
    , view
    )

import Array exposing (Array)
import Debug exposing (toString)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Form as Base
import List.Extra as List


type Form out
    = Form (List (Field out))


type alias BaseForm out =
    Base.Form Key Error out


type alias Key =
    Array Int


type Field out
    = Field
        { field : Key -> Key -> Base.Field Error out
        , view : ViewConfig out -> Key -> List (Html (Msg out))
        }


type alias ViewConfig out =
    { id : String
    , form : Form out
    , base : BaseForm out
    , model : Model out
    }


type alias Error =
    String



-- TEA


type alias Model out =
    Base.Model Key Error out


init : out -> Model out
init emptyOut =
    Base.init emptyOut


type alias Msg out =
    Base.Msg Key out


update : Form out -> Msg out -> Model out -> ( Model out, Base.SubmitTrigger )
update form =
    Base.update (formToBase form)


type alias SubmitStrategy =
    Base.SubmitTrigger


autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitStrategy ) -> ( model, Cmd msg )
autoSubmit =
    Base.autoSubmit


onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitStrategy ) -> ( model, Cmd msg )
onFormSubmit =
    Base.onFormSubmit



-- View


view : (Msg out -> msg) -> String -> Form out -> Model out -> Html msg
view toMsg formId form model =
    let
        config =
            { id = formId
            , form = form
            , base = formToBase form
            , model = model
            }

        children =
            viewFields config Array.empty (formFields form)
    in
    Html.form
        (HA.id formId :: Base.formAttrs identity)
        (stylesheet formId :: children)
        |> Html.map toMsg



-- TODO: should fields come first?


viewFields : ViewConfig out -> Key -> List (Field out) -> List (Html (Msg out))
viewFields config prefix fields =
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

    -- TODO add more CSS
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


formFields : Form out -> List (Field out)
formFields (Form fields) =
    fields


formToBase : Form out -> BaseForm out
formToBase (Form fields) key =
    onBranch fields key key


onLeaf : (String -> Base.Field String out) -> Key -> Key -> Base.Field String out
onLeaf cb fullKey _ =
    cb (keyToString fullKey)


onKeyedLeaf : (Key -> Base.Field String out) -> Key -> Key -> Base.Field String out
onKeyedLeaf cb fullKey _ =
    cb fullKey


onBranch : List (Field out) -> Key -> Key -> Base.Field String out
onBranch fields fullKey key =
    Array.get 0 key
        |> Maybe.andThen (\i -> List.getAt i fields)
        |> Maybe.map
            (\(Field field) ->
                field.field fullKey (Array.slice 1 (Array.length key) key)
            )
        |> Maybe.withDefault Base.emptyField



-- Helpers
-- Helpers


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


type alias Common out =
    { attrs : List (Html.Attribute (Msg out))
    , class : String
    , id : String
    , label : List (Html (Msg out))
    }


emptyCommon : Common out
emptyCommon =
    Common [] "" "" []



-- Text field


type alias TextConfig out =
    { common : Common out
    , autofocus : Bool
    , inputmode : String
    , placeholder : String
    , type_ : String
    }


emptyTextConfig : TextConfig out
emptyTextConfig =
    TextConfig emptyCommon False "" "" "text"


inputField : (String -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
inputField set attrs =
    let
        c =
            attrToConfig emptyTextConfig attrs
    in
    Field
        { field = onLeaf (Base.stringField set)
        , view =
            \{ base, model } key ->
                withLeftLabel c.common.label
                    [ Html.input
                        (HA.type_ c.type_
                            :: Base.attrs identity base key model
                        )
                        []
                    ]
        }



-- Checkbox field


type alias CheckedConfig out =
    { common : Common out
    }


emptyCheckedConfig : CheckedConfig out
emptyCheckedConfig =
    CheckedConfig emptyCommon


checkboxField : (Bool -> out -> out) -> List (Attribute (CheckedConfig out)) -> Field out
checkboxField set attrs =
    let
        c =
            attrToConfig emptyCheckedConfig attrs
    in
    Field
        { field = onLeaf (Base.boolField set)
        , view =
            \{ base, model } key ->
                withRightLabel c.common.label
                    [ Base.checkbox
                        { toMsg = identity
                        , form = base
                        , field = key
                        , value = "y"
                        }
                        model
                    ]
        }



-- Radios field


type alias OptionConfig out =
    { common : Common out
    }


emptyOptionConfig : OptionConfig out
emptyOptionConfig =
    OptionConfig emptyCommon


radioField : (Maybe option -> out -> out) -> (option -> String) -> List option -> List (Attribute (OptionConfig out)) -> Field out
radioField set toString options attrs =
    let
        c =
            attrToConfig emptyOptionConfig attrs

        radioUpdate opt key =
            Base.checkedField
                (set (Just opt))
                (set Nothing)
                (keyToString (Array.slice 0 -1 key))

        radio opt =
            let
                optStr =
                    toString opt
            in
            Field
                { field = onKeyedLeaf (radioUpdate opt)
                , view =
                    \{ base, model } key ->
                        withRightLabel [ Html.text optStr ]
                            [ Base.radio
                                { toMsg = identity
                                , form = base
                                , field = key
                                , value = optStr
                                }
                                model
                            ]
                }
    in
    options
        |> List.foldl (radio >> append) empty
        |> groupField (withLeftLabel c.common.label)



-- Checkboxes


checkboxesField : (option -> out -> out) -> (option -> out -> out) -> (option -> String) -> List option -> List (Attribute (OptionConfig out)) -> Field out
checkboxesField insert remove toString options attrs =
    let
        c =
            attrToConfig emptyOptionConfig attrs

        checkbox opt =
            let
                asStr =
                    toString opt
            in
            Field
                { field = onLeaf (Base.checkedField (insert opt) (remove opt))
                , view =
                    \{ base, model } key ->
                        withRightLabel [ Html.text asStr ]
                            [ Base.checkbox
                                { toMsg = identity
                                , form = base
                                , field = key
                                , value = asStr
                                }
                                model
                            ]
                }
    in
    options
        |> List.foldl (checkbox >> append) empty
        |> groupField (withLeftLabel c.common.label)



-- Select field


selectField : (Maybe option -> out -> out) -> (option -> String) -> (String -> Maybe option) -> List option -> List (Attribute (OptionConfig out)) -> Field out
selectField set toString fromString options attrs =
    let
        c =
            attrToConfig emptyOptionConfig attrs
    in
    Field
        { field = onLeaf (Base.stringField (fromString >> set))
        , view =
            \{ base, model } key ->
                withLeftLabel c.common.label
                    [ options
                        |> List.map toString
                        |> List.map
                            (\value ->
                                Base.option
                                    { form = base
                                    , field = key
                                    , value = value
                                    , label = value
                                    }
                                    model
                            )
                        |> Base.select identity base key
                    ]
        }



-- HTML fields


submit : String -> Field out
submit l =
    htmlField [ Html.button [ HA.type_ "submit" ] [ Html.text l ] ]


htmlField : List (Html (Msg out)) -> Field out
htmlField raw =
    Field
        { field = onLeaf (\_ -> Base.emptyField)
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
groupField wrapper (Form fields) =
    Field
        { field = onBranch fields
        , view =
            \config key ->
                wrapper (viewFields config key fields)
        }


fieldset : String -> Form data -> Field data
fieldset title (Form fields) =
    Field
        { field = onBranch fields
        , view =
            \config key ->
                [ Html.fieldset []
                    (Html.legend [] [ Html.text title ]
                        :: viewFields config key fields
                    )
                ]
        }



-- Common field attributes


type alias WithCommon a out =
    { a | common : Common out }


withCommon : (Common out -> Common out) -> WithCommon a out -> WithCommon a out
withCommon set o =
    { o | common = set o.common }


label : List (Html (Msg out)) -> Attribute (WithCommon c out)
label v =
    withCommon (\c -> { c | label = v })


textLabel : String -> Attribute (WithCommon c out)
textLabel str =
    label [ Html.text str ]


htmlAttribute : Html.Attribute (Msg out) -> Attribute (WithCommon c out)
htmlAttribute attr =
    withCommon (\c -> { c | attrs = c.attrs ++ [ attr ] })


id : String -> Attribute (WithCommon c out)
id =
    HA.id >> htmlAttribute


class : String -> Attribute (WithCommon c out)
class =
    HA.class >> htmlAttribute



-- Field attributes


type alias Attribute config =
    config -> config


attrToConfig : config -> List (Attribute config) -> config
attrToConfig zero attr =
    List.foldl (\fn acc -> fn acc) zero attr


autofocus : Bool -> Attribute { c | autofocus : Bool }
autofocus v o =
    { o | autofocus = v }



-- TODO: define type InputMode = ...


inputmode : String -> Attribute { c | inputmode : String }
inputmode v o =
    { o | inputmode = v }


placeholder : String -> Attribute { c | placeholder : String }
placeholder v o =
    { o | placeholder = v }


type_ : String -> Attribute { c | type_ : String }
type_ v o =
    { o | type_ = v }
