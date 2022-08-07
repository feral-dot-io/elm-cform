module Html.Form exposing
    ( Control
    , Form
    , Model
    , Msg
    , SubmitTrigger
    , attrs
    , autoSubmit
    , checkbox
    , checkedControl
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
type alias Form control out =
    control -> Control out


{-| Represents a form control. Used to create the events of a page's Html attributes
-}
type Control out
    = Control
        { update : String -> Maybe String -> Db out -> Db out
        , name : String
        , value : ControlValue
        }


type ControlValue
    = StringControl
    | CheckedControl String



-- Model


{-| Holds our internal DB tracking a form's current values
-}
type Model out
    = Model (Db out)


{-| Creates an empty model. Requires an empty value for our returned `out` record. This `out` record is never read and not used in the form's HTML -- fields are only set in response to events. You should use `setString` and setChecked\` to set initial values in the form.
-}
init : out -> Model out
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
update : Form control out -> Msg control out -> Model out -> ( Model out, SubmitTrigger )
update form msg model =
    case msg of
        OnInput ctrl val ->
            ( set form ctrl val model, SubmitOnInput )

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
    -> (Model out -> model)
    -> ( Model out, SubmitTrigger )
    -> ( model, Cmd msg )
onSubmit strategies next setter ( model, sub ) =
    let
        oModel =
            setter model
    in
    if List.member sub strategies then
        next oModel (currentOutput model)

    else
        ( oModel, Cmd.none )


{-| Processes return values from form updates. Runs a callback whenever any form input changes. For example used on a search query that does something as the user types.
-}
autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitTrigger ) -> ( model, Cmd msg )
autoSubmit =
    onSubmit [ SubmitOnInput, SubmitOnForm ]


{-| Similiar to autoSubmit: processes form updates. Updates when a form is submitted. For example the user clicks a `<button type="submit">` or presses enter on a text input.
-}
onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitTrigger ) -> ( model, Cmd msg )
onFormSubmit =
    onSubmit [ SubmitOnForm ]



-- Our internal DB


{-| Holds our internal form state.
-}
type alias Db out =
    { out : out
    , state : Dict String String
    }


emptyDB : out -> Db out
emptyDB emptyOut =
    Db emptyOut Dict.empty


{-| Returns the model's current output built up from form events.
-}
currentOutput : Model out -> out
currentOutput (Model db) =
    db.out


{-| Updates our DB and current output current.
-}
updateDb : (Maybe String -> out -> out) -> String -> Maybe String -> Db out -> Db out
updateDb setter name value db =
    { db
        | state =
            case value of
                Just str ->
                    Dict.insert name str db.state

                Nothing ->
                    Dict.remove name db.state
        , out = setter value db.out
    }


{-| Takes a form's control and updates our model with the given value.
-}
set : Form control out -> control -> Maybe String -> Model out -> Model out
set form ctrlKey val (Model db) =
    let
        (Control ctrl) =
            form ctrlKey
    in
    Model (ctrl.update ctrl.name val db)


{-| Sets the value for a string-based control. Used for initial form values.
-}
setString : Form control out -> control -> String -> Model out -> Model out
setString form ctrl val model =
    set form ctrl (Just val) model


{-| Sets the default checked (on / off) state for a checkbox or radio control. Used for initial form values.
-}
setChecked : Form control out -> control -> Bool -> Model out -> Model out
setChecked form ctrlKey checked (Model db) =
    let
        (Control ctrl) =
            form ctrlKey

        -- Retrieve checked value
        val =
            case ctrl.value of
                CheckedControl constant ->
                    if checked then
                        Just constant

                    else
                        Nothing

                StringControl ->
                    Nothing
    in
    Model (ctrl.update ctrl.name val db)



-- Creating a control


{-| Describes a single string control. Takes a field name (used in Html.Attribute.name) and an output setter. The callback is ran on every event related to this control.
-}
stringControl : (String -> out -> out) -> String -> Control out
stringControl setter name =
    Control
        { update = updateDb (Maybe.withDefault "" >> setter)
        , name = name
        , value = StringControl
        }


{-| Similar to stringControl but for a single checked control like a checkboxe or radio. A value must be associated this control which is either set or not (representing the bool in the callback). Radios on the same field should share the same name.
-}
checkedControl : (Bool -> out -> out) -> String -> String -> Control out
checkedControl setter name value =
    Control
        { update = updateDb (\state -> setter (state == Just value))
        , name = name
        , value = CheckedControl value
        }


{-| A control that ignores all events and doesn't output a form's output. The nil or no-op event.
-}
emptyControl : Control out
emptyControl =
    Control
        { update = \_ _ out -> out
        , name = ""
        , value = StringControl
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
eventAttrs : (Msg control out -> msg) -> Form control out -> control -> List (Html.Attribute msg)
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
stateAttrs : Form control out -> control -> Model out -> List (Html.Attribute msg)
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
attrs : (Msg control out -> msg) -> Form control out -> control -> Model out -> List (Html.Attribute msg)
attrs toMsg form ctrl model =
    eventAttrs toMsg form ctrl ++ stateAttrs form ctrl model


{-| Helper function to build a text input `Html.input [Html.type_ "text", ...] []`
-}
textInput : (Msg control out -> msg) -> Form control out -> control -> Model out -> Html msg
textInput toMsg form control model =
    Html.input (HA.type_ "text" :: attrs toMsg form control model) []


{-| Helper function to build a checkbox `Html.input [Html.type_ "checkbox", ...] []`
-}
checkbox : (Msg control out -> msg) -> Form control out -> control -> Model out -> Html msg
checkbox toMsg form control model =
    Html.input (HA.type_ "checkbox" :: attrs toMsg form control model) []


{-| Helper function to build a radio `Html.input [Html.type_ "radio", ...] []`
-}
radio : (Msg control out -> msg) -> Form control out -> control -> Model out -> Html msg
radio toMsg form control model =
    Html.input (HA.type_ "radio" :: attrs toMsg form control model) []


{-| Helper function to build a select `Html.select [...] options`
-}
select : (Msg control out -> msg) -> Form control out -> control -> List (Html msg) -> Html msg
select toMsg form control options =
    Html.select (eventAttrs toMsg form control) options
