module Composable.Form exposing
    ( Field
    , Form
    , Model
    , Msg
    , SubmitStrategy
    , append
    , appendIf
    , autoSubmit
    , checkboxField
    , checkboxesForm
    , column
    , empty
    , fieldset
    , init
    , onFormSubmit
    , radiosForm
    , row
    , submit
    , textField
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as HA
import Html.Form as Base
import List.Extra as List


type Form out
    = Form (List (Field out))


type alias BaseForm out =
    Base.Form Key out


type alias Key =
    List Int


type Field out
    = Field
        { control : Key -> Key -> Base.Control out
        , view : FormConfig out -> Key -> List (Html (Msg out))
        }


type alias FormConfig out =
    { id : String
    , form : BaseForm out
    , model : Model out
    }



-- TEA


type alias Model out =
    Base.Model out


init : out -> Model out
init emptyOut =
    Base.init emptyOut


type alias Msg out =
    Base.Msg Key out


update : Form out -> Msg out -> Model out -> ( Model out, Base.SubmitStrategy )
update form =
    Base.update (toBaseForm form)


type alias SubmitStrategy =
    Base.SubmitStrategy


autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitStrategy ) -> ( model, Cmd msg )
autoSubmit =
    Base.autoSubmit


onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitStrategy ) -> ( model, Cmd msg )
onFormSubmit =
    Base.onFormSubmit



-- View


view : (Msg out -> msg) -> String -> Form out -> Model out -> Html msg
view toMsg id form model =
    let
        config =
            { id = id
            , form = toBaseForm form
            , model = model
            }

        children =
            formFields form
                |> viewFields config []
    in
    Html.form
        (HA.id id :: Base.formAttrs identity)
        (stylesheet id :: children)
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
stylesheet id =
    let
        rule str =
            "#" ++ id ++ " " ++ str
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


toBaseForm : Form out -> BaseForm out
toBaseForm (Form fields) key =
    case key of
        i :: rem ->
            List.getAt i fields
                |> Maybe.map
                    (\(Field field) ->
                        field.control key rem
                    )
                |> Maybe.withDefault Base.emptyControl

        [] ->
            Base.emptyControl


toBaseForm_ : List (Field out) -> Key -> BaseForm out
toBaseForm_ fields name key =
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


onLeaf : (String -> Base.Control out) -> Key -> Key -> Base.Control out
onLeaf cb name _ =
    cb (keyToString name)


nestedControl : List (Field out) -> Key -> Key -> Base.Control out
nestedControl fields =
    toBaseForm_ fields


keyToString : Key -> String
keyToString key =
    List.map String.fromInt key
        |> String.join "-"



-- Text field


textField : String -> (String -> out -> out) -> Field out
textField type_ set =
    Field
        { control = onLeaf (\name -> Base.stringControl name set)
        , view =
            \{ form, model } ctrl ->
                [ Html.input (HA.type_ type_ :: Base.attrs identity form ctrl model) []
                ]
        }



-- Checkbox field


checkboxField : (Bool -> out -> out) -> Field out
checkboxField set =
    Field
        { control = onLeaf (\name -> Base.checkedControl name "y" set)
        , view =
            \{ form, model } ctrl ->
                [ Base.checkbox identity form ctrl model
                ]
        }



-- Radios field


radiosForm : (Maybe option -> out -> out) -> (option -> String) -> List option -> Form out
radiosForm set toString options =
    let
        radio opt =
            Field
                { control =
                    \key _ ->
                        Base.checkedControl
                            (keyToString (Maybe.withDefault [] (List.init key)))
                            (toString opt)
                            (\v ->
                                set
                                    (if v then
                                        Just opt

                                     else
                                        Nothing
                                    )
                            )
                , view =
                    \{ form, model } ctrl ->
                        [ Base.radio identity form ctrl model
                        ]
                }
    in
    List.foldl (radio >> append) empty options



-- Checkboxes


checkboxesForm : (option -> out -> out) -> (option -> out -> out) -> (option -> String) -> List option -> Form out
checkboxesForm insert remove toString options =
    let
        checkbox opt =
            Field
                { control =
                    onLeaf
                        (\key ->
                            Base.checkedControl key
                                (toString opt)
                                (\v ->
                                    (if v then
                                        insert

                                     else
                                        remove
                                    )
                                        opt
                                )
                        )
                , view =
                    \{ form, model } ctrl ->
                        [ Base.checkbox identity form ctrl model
                        ]
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
groupField class (Form fields) =
    Field
        { control = nestedControl fields
        , view =
            \config key ->
                [ Html.div [ HA.class class ]
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
