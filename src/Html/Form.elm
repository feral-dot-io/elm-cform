module Html.Form exposing
    ( Control
    , ControlState
    , Form
    , Model
    , Msg
    , SubmitTrigger
    , attrs
    , autoSubmit
    , checkbox
    , checkedControl
    , controlState
    , emptyControl
    , formAttrs
    , init
    , onFormSubmit
    , radio
    , select
    , setChecked
    , setString
    , stringControl
    , textInput
    , update
    , withValidator
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
type alias Form control err out =
    control -> Control err out


{-| Represents a form control. Used to create the events of a page's Html attributes
-}
type Control err out
    = Control
        { update : Maybe String -> out -> Result err out
        , name : String
        , value : ControlValue
        }


type ControlValue
    = StringControl
    | CheckedControl String



-- Model


{-| Holds our internal DB tracking a form's current values
-}
type Model control err out
    = Model (Db control err out)


{-| Creates an empty model. Requires an empty value for our returned `out` record. This `out` record is never read and not used in the form's HTML -- fields are only set in response to events. You should use `setString` and setChecked\` to set initial values in the form.
-}
init : out -> Model control err out
init emptyOut =
    Model (emptyDB emptyOut)



-- Update


{-| Holds our form events.
-}
type Msg control out
    = OnInput control (Maybe String)
    | OnBlur control
    | OnSubmit


{-| Processes our form events and updates our model. Returns a new model and a submission trigger. The caller should use `autoSubmit` and `onFormSubmit` to process form submissions. For example:

    Form.update form formMsg formModel
        |> Form.autoSubmit onSubmit (\f -> { model | formModel = f })

-}
update : Form control err out -> Msg control out -> Model control err out -> ( Model control err out, SubmitTrigger )
update form msg model =
    case msg of
        OnInput ctrl val ->
            ( updateModel form ctrl val model, SubmitOnInput )

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
    -> (Model control err out -> model)
    -> ( Model control err out, SubmitTrigger )
    -> ( model, Cmd msg )
onSubmit strategies next setter ( model, sub ) =
    let
        (Model db) =
            model
    in
    if List.member sub strategies && Dict.isEmpty db.errors then
        next
            (setter (resetTouched model))
            (currentOutput model)

    else
        ( setter model, Cmd.none )


{-| Processes return values from form updates. Runs a callback whenever any form input changes. For example used on a search query that does something as the user types.
-}
autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model control err out -> model) -> ( Model control err out, SubmitTrigger ) -> ( model, Cmd msg )
autoSubmit =
    onSubmit [ SubmitOnInput, SubmitOnForm ]


{-| Similiar to autoSubmit: processes form updates. Updates when a form is submitted. For example the user clicks a `<button type="submit">` or presses enter on a text input.
-}
onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model control err out -> model) -> ( Model control err out, SubmitTrigger ) -> ( model, Cmd msg )
onFormSubmit =
    onSubmit [ SubmitOnForm ]



-- Our internal DB


{-| Holds our internal form state.
-}
type alias Db control err out =
    { out : out
    , touched : Dict String control
    , state : Dict String String
    , errors : Dict String err
    }


emptyDB : out -> Db control err out
emptyDB emptyOut =
    Db emptyOut Dict.empty Dict.empty Dict.empty


{-| Returns the model's current output built up from form events.
-}
currentOutput : Model control err out -> out
currentOutput (Model db) =
    db.out


resetTouched : Model control err out -> Model control err out
resetTouched (Model db) =
    Model { db | touched = Dict.empty }


updateModel : Form control err out -> control -> Maybe String -> Model control err out -> Model control err out
updateModel form ctrlKey value (Model db) =
    let
        (Control ctrl) =
            form ctrlKey

        db2 =
            { db
                | touched = Dict.insert ctrl.name ctrlKey db.touched
                , errors = Dict.remove ctrl.name db.errors
                , state =
                    case value of
                        Just str ->
                            Dict.insert ctrl.name str db.state

                        Nothing ->
                            Dict.remove ctrl.name db.state
            }

        db3 =
            case ctrl.update value db.out of
                Ok out ->
                    { db2 | out = out }

                Err err ->
                    { db2 | errors = Dict.insert ctrl.name err db2.errors }
    in
    Model db3


{-| Sets the value for a string-based control. Used for initial form values.
-}
setString : Form control err out -> control -> String -> Model control err out -> Model control err out
setString form ctrl val model =
    updateModel form ctrl (Just val) model


{-| Sets the default checked (on / off) state for a checkbox or radio control. Used for initial form values.
-}
setChecked : Form control err out -> control -> Bool -> Model control err out -> Model control err out
setChecked form ctrlKey checked model =
    let
        (Control { value }) =
            form ctrlKey

        -- Retrieve checked value
        val =
            case value of
                CheckedControl constant ->
                    if checked then
                        Just constant

                    else
                        Nothing

                StringControl ->
                    Nothing
    in
    updateModel form ctrlKey val model



-- Creating a control


{-| Describes a single string control. Takes a field name (used in Html.Attribute.name) and an output setter. The callback is ran on every event related to this control.
-}
stringControl : (String -> out -> out) -> String -> Control err out
stringControl setter name =
    Control
        { update = \state out -> Ok (setter (Maybe.withDefault "" state) out)
        , name = name
        , value = StringControl
        }


{-| Similar to stringControl but for a single checked control like a checkbox or radio. A value must be associated this control which can be checked or not. For radio buttons, this distinguishes it from others with the same name.
-}
checkedControl : (out -> out) -> (out -> out) -> String -> String -> Control err out
checkedControl checked unchecked name value =
    Control
        { update =
            \state out ->
                if state == Just value then
                    Ok (checked out)

                else
                    Ok (unchecked out)
        , name = name
        , value = CheckedControl value
        }


{-| A control that ignores all events and doesn't output a form's output. The nil or no-op event.
-}
emptyControl : Control err out
emptyControl =
    Control
        { update = \_ out -> Ok out
        , name = ""
        , value = StringControl
        }


{-| Wraps a control with a validator
-}
withValidator : (String -> out -> Result err out) -> Control err out -> Control err out
withValidator validator (Control ctrl) =
    Control
        { ctrl
          -- Wrap update
            | update =
                \state out ->
                    validator (Maybe.withDefault "" state) out
                        |> Result.andThen (ctrl.update state)
        }



-- View (HTML attributes)


{-| Attributes to be used on a Html.form. For example `Html.form (Form.formAttrs FormMsg) [ ... ]`
-}
formAttrs : (Msg control out -> msg) -> List (Html.Attribute msg)
formAttrs toMsg =
    [ HE.onSubmit (toMsg OnSubmit) ]


{-| Uses targetChecked to send a control's value or not. An empty string is not special so we use a Maybe here.
-}
checkedDecoder : String -> JD.Decoder (Maybe String)
checkedDecoder constant =
    JD.map
        -- We're expecting an on/off: send whole value or not
        (\checked ->
            if checked then
                Just constant

            else
                Nothing
        )
        HE.targetChecked


{-| Sends an event's target value. Always send a string: an empty string is not a missing value.
-}
alwaysTargetValueDecoder : JD.Decoder (Maybe String)
alwaysTargetValueDecoder =
    -- Always send target's value
    JD.map Just HE.targetValue


{-| Builds our event-based attributes.
-}
eventAttrs : (Msg control out -> msg) -> Form control err out -> control -> List (Html.Attribute msg)
eventAttrs toMsg form ctrlKey =
    let
        (Control ctrl) =
            form ctrlKey

        eventDecoder =
            case ctrl.value of
                StringControl ->
                    alwaysTargetValueDecoder

                CheckedControl value ->
                    checkedDecoder value
    in
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( toMsg (OnInput ctrlKey event), True ))
            eventDecoder
        )
    , HE.onBlur (toMsg (OnBlur ctrlKey))
    , HA.name ctrl.name
    ]


{-| Builds our state-based attributes.
-}
stateAttrs : Form control err out -> control -> Model control err out -> List (Html.Attribute msg)
stateAttrs form ctrlKey (Model db) =
    let
        (Control ctrl) =
            form ctrlKey

        currentState =
            Dict.get ctrl.name db.state
                |> Maybe.withDefault ""
    in
    case ctrl.value of
        StringControl ->
            [ HA.value currentState
            ]

        CheckedControl value ->
            [ HA.value value
            , HA.checked (currentState == value)
            ]


{-| HTML attributes to put on a control's HTML. Includes our current state and events.
-}
attrs : (Msg control out -> msg) -> Form control err out -> control -> Model control err out -> List (Html.Attribute msg)
attrs toMsg form ctrl model =
    eventAttrs toMsg form ctrl ++ stateAttrs form ctrl model


{-| Helper function to build a text input `Html.input [Html.type_ "text", ...] []`
-}
textInput : (Msg control out -> msg) -> Form control err out -> control -> Model control err out -> Html msg
textInput toMsg form control model =
    Html.input (HA.type_ "text" :: attrs toMsg form control model) []


{-| Helper function to build a checkbox `Html.input [Html.type_ "checkbox", ...] []`
-}
checkbox : (Msg control out -> msg) -> Form control err out -> control -> Model control err out -> Html msg
checkbox toMsg form control model =
    Html.input (HA.type_ "checkbox" :: attrs toMsg form control model) []


{-| Helper function to build a radio `Html.input [Html.type_ "radio", ...] []`
-}
radio : (Msg control out -> msg) -> Form control err out -> control -> Model control err out -> Html msg
radio toMsg form control model =
    Html.input (HA.type_ "radio" :: attrs toMsg form control model) []


{-| Helper function to build a select `Html.select [...] options`
-}
select : (Msg control out -> msg) -> Form control err out -> control -> List (Html msg) -> Html msg
select toMsg form control options =
    Html.select (eventAttrs toMsg form control) options



-- Field state


type alias ControlState err =
    { name : String
    , value : String
    , error : Maybe err
    , changed : Bool
    , changedSinceSubmit : Bool
    }


controlState : Form control err out -> control -> Model control err out -> ControlState err
controlState form ctrlKey (Model db) =
    let
        (Control ctrl) =
            form ctrlKey

        state =
            Dict.get ctrl.name db.state
    in
    { name = ctrl.name
    , value = Maybe.withDefault "" state
    , error = Dict.get ctrl.name db.errors
    , changed = state /= Nothing
    , changedSinceSubmit = Dict.member ctrl.name db.touched
    }
