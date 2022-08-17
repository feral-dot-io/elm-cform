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
    , checkboxesForm
    , class
    , column
    , empty
    , fieldset
    , htmlAttribute
    , id
    , init
    , inputmode
    , label
    , onFormSubmit
    , placeholder
    , radiosForm
    , row
    , submit
    , textField
    , textLabel
    , type_
    , update
    , view
    )

import Debug exposing (toString)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Form as Base
import List.Extra as List


type Form out
    = Form (List (Field out))


type alias BaseForm out =
    Base.Form Key String out


type alias Key =
    List Int


type Field out
    = Field
        { control : Key -> Key -> Base.Control String out
        , view : FormConfig out -> Key -> List (Html (Msg out))
        }


type alias FormConfig out =
    { topId : String
    , form : BaseForm out
    , model : Model out
    }



-- TEA


type alias Model out =
    Base.Model Key String out


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
            { topId = formId
            , form = formToBase form
            , model = model
            }

        children =
            formFields form
                |> viewFields config []
    in
    Html.form
        (HA.id formId :: Base.formAttrs identity)
        (stylesheet formId :: children)
        |> Html.map toMsg


viewFields : FormConfig out -> Key -> List (Field out) -> List (Html (Msg out))
viewFields config prefix fields =
    let
        viewField i (Field field) =
            Html.div [ HA.class "field" ]
                (field.view config (List.append prefix [ i ]))
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
    fieldsToBase fields key key


fieldsToBase : List (Field out) -> Key -> BaseForm out
fieldsToBase fields name key =
    case key of
        i :: rem ->
            List.getAt i fields
                |> Maybe.map
                    (\(Field field) ->
                        field.control name rem
                    )
                |> Maybe.withDefault Base.emptyControl

        [] ->
            Base.emptyControl


onLeaf : (String -> Base.Control String out) -> Key -> Key -> Base.Control String out
onLeaf cb name _ =
    cb (keyToString name)


nestedControl : List (Field out) -> Key -> Key -> Base.Control String out
nestedControl fields =
    fieldsToBase fields



-- Helpers
-- Helpers


keyToString : Key -> String
keyToString key =
    List.map String.fromInt key
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


textField : (String -> out -> out) -> List (Attribute (TextConfig out)) -> Field out
textField set attrs =
    let
        c =
            attrToConfig emptyTextConfig attrs
    in
    Field
        { control = onLeaf (Base.stringControl set)
        , view =
            \{ form, model } ctrl ->
                withLeftLabel c.common.label
                    [ Html.input
                        (HA.type_ c.type_
                            :: Base.attrs identity form ctrl model
                        )
                        []
                    ]
        }



-- Checkbox field


type alias CheckboxConfig out =
    { common : Common out
    }


emptyCheckboxConfig : CheckboxConfig out
emptyCheckboxConfig =
    CheckboxConfig emptyCommon


checkboxField : (Bool -> out -> out) -> List (Attribute (CheckboxConfig out)) -> Field out
checkboxField set attrs =
    let
        c =
            attrToConfig emptyCheckboxConfig attrs
    in
    Field
        { control =
            onLeaf
                (\name ->
                    Base.checkedControl (set True)
                        (set False)
                        name
                        "y"
                )
        , view =
            \{ form, model } ctrl ->
                withRightLabel c.common.label
                    [ Base.checkbox identity form ctrl model ]
        }



-- Radios field


radiosForm : (Maybe option -> out -> out) -> (option -> String) -> List option -> Form out
radiosForm set toString options =
    let
        radio opt =
            let
                asStr =
                    toString opt
            in
            Field
                { control =
                    \key _ ->
                        Base.checkedControl
                            (set (Just opt))
                            (set Nothing)
                            (keyToString (Maybe.withDefault [] (List.init key)))
                            asStr
                , view =
                    \{ form, model } ctrl ->
                        withRightLabel [ Html.text asStr ]
                            [ Base.radio identity form ctrl model ]
                }
    in
    List.foldl (radio >> append) empty options



-- Checkboxes


checkboxesForm : (option -> out -> out) -> (option -> out -> out) -> (option -> String) -> List option -> Form out
checkboxesForm insert remove toString options =
    let
        checkbox opt =
            let
                asStr =
                    toString opt
            in
            Field
                { control =
                    onLeaf
                        (\key ->
                            Base.checkedControl (insert opt) (remove opt) key asStr
                        )
                , view =
                    \{ form, model } ctrl ->
                        withRightLabel [ Html.text asStr ]
                            [ Base.checkbox identity form ctrl model ]
                }
    in
    List.foldl (checkbox >> append) empty options



-- HTML fields


submit : String -> Field out
submit l =
    htmlField [ Html.button [ HA.type_ "submit" ] [ Html.text l ] ]


htmlField : List (Html (Msg out)) -> Field out
htmlField raw =
    Field
        { control = onLeaf (\_ -> Base.emptyControl)
        , view = \_ _ -> raw
        }



-- Grouping (nested: Form to Field) fields


row : Form out -> Field out
row form =
    groupField "row" form


column : Form out -> Field out
column form =
    groupField "column" form


groupField : String -> Form out -> Field out
groupField groupClass (Form fields) =
    Field
        { control = nestedControl fields
        , view =
            \config key ->
                [ Html.div [ HA.class groupClass ]
                    (viewFields config key fields)
                ]
        }


fieldset : String -> Form data -> Field data
fieldset title (Form fields) =
    Field
        { control = nestedControl fields
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
