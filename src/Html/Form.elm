module Html.Form exposing
    ( Control
    , Field
    , Form
    , Model
    , Msg
    , SubmitTrigger
    , attrs
    , autoSubmit
    , boolField
    , checkbox
    , checkedAttrs
    , checkedField
    , controlState
    , emptyField
    , fieldState
    , formAttrs
    , init
    , onFormSubmit
    , option
    , optionAttrs
    , optionsField
    , radio
    , rawField
    , select
    , selectAttrs
    , setChecked
    , setString
    , stringField
    , textInput
    , update
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD



{-
   TODO
    - default values: init vs form builder
        - usually comes with data, makes accessing form harder if wrapped in WebData
        - free form in init vs typed in form builder
    - unstructured html vs structured form builder
        - unstructured lacks types
        - structured limits flexibility
-}
-- Types


{-| The Form type holds a conversion function that maps our `control` type to a `Control`
-}
type alias Form field err out =
    field -> Field err out


{-| Represents a form control. Used to create the events of a page's Html attributes
-}
type Field err out
    = Field
        { update : Maybe String -> out -> Result err out
        }


type alias Control =
    String



-- Model


{-| Holds our internal DB tracking a form's current values
-}
type Model field err out
    = Model (Db field err out)


{-| Creates an empty model. Requires an empty value for our returned `out` record. This `out` record is never read and not used in the form's HTML -- fields are only set in response to events. You should use `setString` and setChecked\` to set initial values in the form.
-}
init : out -> Model field err out
init emptyOut =
    Model (emptyDB emptyOut)



-- Update


{-| Holds our form events.
-}
type Msg field out
    = OnInput (Event field)
    | OnBlur field
    | OnSubmit


{-| Processes our form events and updates our model. Returns a new model and a submission trigger. The caller should use `autoSubmit` and `onFormSubmit` to process form submissions. For example:

    Form.update form formMsg formModel
        |> Form.autoSubmit onSubmit (\f -> { model | formModel = f })

-}
update : Form field err out -> Msg field out -> Model field err out -> ( Model field err out, SubmitTrigger )
update form msg model =
    case msg of
        OnInput event ->
            ( updateModel form event model, SubmitOnInput )

        OnBlur _ ->
            ( model, SubmitOnBlur )

        OnSubmit ->
            ( model, SubmitOnForm )



-- Submission handling


{-| Represents a submission trigger. For example if the user hits "submit" on a form we'd see a form level submission trigger.
-}
type SubmitTrigger
    = SubmitOnInput
    | SubmitOnBlur
    | SubmitOnForm


onSubmit :
    List SubmitTrigger
    -> (model -> out -> ( model, Cmd msg ))
    -> (Model field err out -> model)
    -> ( Model field err out, SubmitTrigger )
    -> ( model, Cmd msg )
onSubmit strategies next setter ( model, sub ) =
    let
        (Model db) =
            model
    in
    if List.member sub strategies && List.isEmpty db.errors then
        next
            (setter (resetSinceSubmit model))
            (currentOutput model)

    else
        ( setter model, Cmd.none )


{-| Processes return values from form updates. Runs a callback whenever any form input changes. For example used on a search query that does something as the user types.
-}
autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model field err out -> model) -> ( Model field err out, SubmitTrigger ) -> ( model, Cmd msg )
autoSubmit =
    onSubmit [ SubmitOnInput, SubmitOnForm ]


{-| Similiar to autoSubmit: processes form updates. Updates when a form is submitted. For example the user clicks a `<button type="submit">` or presses enter on a text input.
-}
onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model field err out -> model) -> ( Model field err out, SubmitTrigger ) -> ( model, Cmd msg )
onFormSubmit =
    onSubmit [ SubmitOnForm ]



-- Our internal DB


{-| Holds our internal form state.
-}
type alias Db field err out =
    { out : out
    , state : Dict String String
    , errors : List ( field, err )
    , touched : List field
    , touchedSinceSubmit : List field
    , changed : List field
    , changedSinceSubmit : List field
    }


emptyDB : out -> Db field err out
emptyDB emptyOut =
    Db emptyOut Dict.empty [] [] [] [] []


{-| Returns the model's current output built up from form events.
-}
currentOutput : Model field err out -> out
currentOutput (Model db) =
    db.out


resetSinceSubmit : Model field err out -> Model field err out
resetSinceSubmit (Model db) =
    Model { db | touchedSinceSubmit = [], changedSinceSubmit = [] }


updateModel : Form field err out -> Event field -> Model field err out -> Model field err out
updateModel form event (Model db) =
    let
        (Field field) =
            form event.field

        addField s =
            if List.member event.field s then
                s

            else
                event.field :: s

        -- Update our tracking state
        db2 =
            { db
              -- Touched / changed
                | touched = addField db.touched
                , touchedSinceSubmit = addField db.touchedSinceSubmit
                , changed = addField db.changed
                , changedSinceSubmit = addField db.changedSinceSubmit

                -- State change
                , state =
                    case event.value of
                        Just str ->
                            Dict.insert event.control str db.state

                        Nothing ->
                            Dict.remove event.control db.state
                , errors =
                    List.filter (\( check, _ ) -> check /= event.field)
                        db.errors
            }
    in
    Model
        -- Update our output or error
        (case field.update event.value db2.out of
            Ok out ->
                { db2 | out = out }

            Err err ->
                { db2 | errors = ( event.field, err ) :: db2.errors }
        )


{-| Sets the value for a string-based control. Used for initial form values.
-}
setString : Form field err out -> field -> Control -> String -> Model field err out -> Model field err out
setString form field ctrl val =
    updateModel form
        { field = field
        , control = ctrl
        , value = Just val
        }


setChecked : Form field err out -> field -> Control -> String -> Bool -> Model field err out -> Model field err out
setChecked form field ctrl val checked =
    updateModel form
        { field = field
        , control = ctrl
        , value =
            if checked then
                Just val

            else
                Nothing
        }



-- Creating a control


rawField : (Maybe String -> out -> out) -> Field err out
rawField set =
    Field { update = \value out -> Ok (set value out) }


stringField : (String -> out -> out) -> Field err out
stringField set =
    Field { update = \value out -> Ok (set (Maybe.withDefault "" value) out) }


boolField : (Bool -> out -> out) -> Field err out
boolField set =
    Field { update = \value out -> Ok (set (value /= Nothing) out) }


checkedField : (out -> out) -> (out -> out) -> Field err out
checkedField on off =
    boolField
        (\checked out ->
            if checked then
                on out

            else
                off out
        )


optionsField : (List option -> out -> out) -> (out -> List option) -> option -> Field err out
optionsField set get opt =
    boolField
        (\checked out ->
            let
                updated options =
                    if checked then
                        opt :: options

                    else
                        List.filter ((/=) opt) options
            in
            set (updated (get out)) out
        )


{-| A control that ignores all events and doesn't output a form's output. The nil or no-op event.
-}
emptyField : Field err out
emptyField =
    Field { update = \_ out -> Ok out }



-- State handling


type alias FieldState field err =
    { field : field
    , error : Maybe err
    , touched : Bool
    , touchedSinceSubmit : Bool
    , changed : Bool
    , changedSinceSubmit : Bool
    }


fieldState : field -> Model field err out -> FieldState field err
fieldState fieldKey (Model db) =
    { field = fieldKey
    , error =
        db.errors
            |> List.filterMap
                (\( check, err ) ->
                    if check == fieldKey then
                        Just err

                    else
                        Nothing
                )
            |> List.head
    , touched = List.member fieldKey db.touched
    , touchedSinceSubmit = List.member fieldKey db.touchedSinceSubmit
    , changed = List.member fieldKey db.changed
    , changedSinceSubmit = List.member fieldKey db.changedSinceSubmit
    }


type alias ControlState =
    { control : Control
    , value : Maybe String
    , touched : Bool
    , touchedSinceSubmit : Bool
    , changed : Bool
    , changedSinceSubmit : Bool
    }


controlValue : Control -> Model field err out -> Maybe String
controlValue ctrl (Model db) =
    Dict.get ctrl db.state


controlState : field -> Control -> Model field err out -> ControlState
controlState fieldKey ctrl (Model db) =
    { control = ctrl
    , value = controlValue ctrl (Model db)
    , touched = List.member fieldKey db.touched
    , touchedSinceSubmit = List.member fieldKey db.touchedSinceSubmit
    , changed = List.member fieldKey db.changed
    , changedSinceSubmit = List.member fieldKey db.changedSinceSubmit
    }



-- View's base elements


{-| Attributes to be used on a Html.form. For example `Html.form (Form.formAttrs FormMsg) [ ... ]`
-}
formAttrs : (Msg field out -> msg) -> List (Html.Attribute msg)
formAttrs toMsg =
    [ HE.onSubmit (toMsg OnSubmit) ]


type alias Event field =
    { field : field
    , control : Control
    , value : Maybe String
    }


eventDecoder : field -> JD.Decoder Bool -> JD.Decoder (Event field)
eventDecoder field getChecked =
    JD.map2 (Event field)
        (JD.at [ "target", "name" ] JD.string)
        (JD.map2
            (\checked value ->
                if checked then
                    Just value

                else
                    Nothing
            )
            getChecked
            HE.targetValue
        )


stringEventDecoder : field -> JD.Decoder (Event field)
stringEventDecoder field =
    eventDecoder field (JD.succeed True)


checkedEventDecoder : field -> JD.Decoder (Event field)
checkedEventDecoder field =
    eventDecoder field HE.targetChecked


attrs : (Msg field out -> msg) -> field -> Control -> Model field err out -> List (Html.Attribute msg)
attrs toMsg field ctrl model =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( toMsg (OnInput event), True ))
            (stringEventDecoder field)
        )
    , HE.onBlur (toMsg (OnBlur field))
    , HA.name ctrl
    , HA.value (controlValue ctrl model |> Maybe.withDefault "")
    ]


checkedAttrs : (Msg field out -> msg) -> field -> Control -> String -> Model field err out -> List (Html.Attribute msg)
checkedAttrs toMsg fieldKey ctrl value model =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( toMsg (OnInput event), True ))
            (checkedEventDecoder fieldKey)
        )
    , HE.onBlur (toMsg (OnBlur fieldKey))
    , HA.name ctrl
    , HA.value value
    , HA.checked (controlValue ctrl model == Just value)
    ]


optionAttrs : Control -> String -> Model field err out -> List (Html.Attribute msg)
optionAttrs ctrl value model =
    [ HA.value value
    , HA.selected (controlValue ctrl model == Just value)
    ]


selectAttrs : (Msg field out -> msg) -> field -> Control -> List (Html.Attribute msg)
selectAttrs toMsg fieldKey ctrl =
    [ HE.on "change"
        (JD.map (OnInput >> toMsg)
            (stringEventDecoder fieldKey)
        )
    , HE.onBlur (toMsg (OnBlur fieldKey))
    , HA.name ctrl
    ]



-- Helper functions for Html


{-| Helper function to build a text input `Html.input [Html.type_ "text", ...] []`
-}
textInput :
    { toMsg : Msg field out -> msg
    , field : field
    , control : Control
    }
    -> Model field err out
    -> Html msg
textInput { toMsg, field, control } model =
    Html.input (HA.type_ "text" :: attrs toMsg field control model) []


{-| Helper function to build a checkbox `Html.input [Html.type_ "checkbox", ...] []`
-}
checkbox :
    { toMsg : Msg field out -> msg
    , field : field
    , control : Control
    , value : String
    }
    -> Model field err out
    -> Html msg
checkbox { toMsg, field, control, value } model =
    Html.input (HA.type_ "checkbox" :: checkedAttrs toMsg field control value model) []


{-| Helper function to build a radio `Html.input [Html.type_ "radio", ...] []`
-}
radio :
    { toMsg : Msg field out -> msg
    , field : field
    , control : Control
    , value : String
    }
    -> Model field err out
    -> Html msg
radio { toMsg, field, control, value } model =
    Html.input (HA.type_ "radio" :: checkedAttrs toMsg field control value model) []


option :
    { control : Control
    , value : String
    , label : String
    }
    -> Model field err out
    -> Html msg
option { control, value, label } model =
    Html.option
        (optionAttrs control value model)
        [ Html.text label ]


select : (Msg field out -> msg) -> field -> Control -> List (Html msg) -> Html msg
select toMsg field control options =
    Html.select (selectAttrs toMsg field control) options
