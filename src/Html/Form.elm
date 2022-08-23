module Html.Form exposing
    ( Field
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
    , setValue
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
        { name : String
        , update : Maybe String -> out -> Result err out
        }



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
                            Dict.insert field.name str db.state

                        Nothing ->
                            Dict.remove field.name db.state
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


setValue : Form field err out -> field -> Maybe String -> Model field err out -> Model field err out
setValue form field value =
    updateModel form
        { field = field
        , value = value
        }


setString : Form field err out -> field -> String -> Model field err out -> Model field err out
setString form field value =
    setValue form field (Just value)


setChecked : Form field err out -> field -> Bool -> Model field err out -> Model field err out
setChecked form field checked =
    setValue form
        field
        (if checked then
            Just "y"

         else
            Nothing
        )



-- Creating a control


rawField : (Maybe String -> out -> out) -> String -> Field err out
rawField set name =
    Field
        { name = name
        , update = \value out -> Ok (set value out)
        }


stringField : (String -> out -> out) -> String -> Field err out
stringField set =
    rawField (Maybe.withDefault "" >> set)


boolField : (Bool -> out -> out) -> String -> Field err out
boolField set =
    rawField (\value out -> set (value /= Nothing) out)


checkedField : (out -> out) -> (out -> out) -> String -> Field err out
checkedField on off =
    boolField
        (\checked out ->
            if checked then
                on out

            else
                off out
        )


optionsField : (List option -> out -> out) -> (out -> List option) -> String -> option -> Field err out
optionsField set get name opt =
    checkedField
        (\out -> set (opt :: get out) out)
        (\out -> set (List.filter ((/=) opt) (get out)) out)
        name


emptyField : Field err out
emptyField =
    Field { name = "", update = \_ out -> Ok out }



-- State handling


type alias FieldState field err =
    { field : field
    , value : Maybe String
    , error : Maybe err
    , touched : Bool
    , touchedSinceSubmit : Bool
    , changed : Bool
    , changedSinceSubmit : Bool
    }


fieldName : Form field err out -> field -> String
fieldName form fieldKey =
    let
        (Field field) =
            form fieldKey
    in
    field.name


fieldValue : Form field err out -> field -> Model field err out -> Maybe String
fieldValue form field (Model db) =
    Dict.get (fieldName form field) db.state


fieldState : Form field err out -> field -> Model field err out -> FieldState field err
fieldState form field (Model db) =
    { field = field
    , value = fieldValue form field (Model db)
    , error =
        db.errors
            |> List.filterMap
                (\( check, err ) ->
                    if check == field then
                        Just err

                    else
                        Nothing
                )
            |> List.head
    , touched = List.member field db.touched
    , touchedSinceSubmit = List.member field db.touchedSinceSubmit
    , changed = List.member field db.changed
    , changedSinceSubmit = List.member field db.changedSinceSubmit
    }



-- View's base elements


{-| Attributes to be used on a Html.form. For example `Html.form (Form.formAttrs FormMsg) [ ... ]`
-}
formAttrs : (Msg field out -> msg) -> List (Html.Attribute msg)
formAttrs toMsg =
    [ HE.onSubmit (toMsg OnSubmit) ]


type alias Event field =
    { field : field
    , value : Maybe String
    }


eventDecoder : field -> JD.Decoder Bool -> JD.Decoder (Event field)
eventDecoder field getChecked =
    JD.map (Event field)
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


attrs : (Msg field out -> msg) -> Form field err out -> field -> Model field err out -> List (Html.Attribute msg)
attrs toMsg form field model =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( toMsg (OnInput event), True ))
            (stringEventDecoder field)
        )
    , HE.onBlur (toMsg (OnBlur field))
    , HA.name (fieldName form field)
    , HA.value (fieldValue form field model |> Maybe.withDefault "")
    ]


checkedAttrs : (Msg field out -> msg) -> Form field err out -> field -> String -> Model field err out -> List (Html.Attribute msg)
checkedAttrs toMsg form field value model =
    [ HE.stopPropagationOn "input"
        (JD.map
            (\event -> ( toMsg (OnInput event), True ))
            (checkedEventDecoder field)
        )
    , HE.onBlur (toMsg (OnBlur field))
    , HA.name (fieldName form field)
    , HA.value value
    , HA.checked (fieldValue form field model == Just value)
    ]


optionAttrs : Form field err out -> field -> String -> Model field err out -> List (Html.Attribute msg)
optionAttrs form field value model =
    [ HA.value value
    , HA.selected (fieldValue form field model == Just value)
    ]


selectAttrs : (Msg field out -> msg) -> Form field err out -> field -> List (Html.Attribute msg)
selectAttrs toMsg form field =
    [ HE.on "change"
        (JD.map (OnInput >> toMsg)
            (stringEventDecoder field)
        )
    , HE.onBlur (toMsg (OnBlur field))
    , HA.name (fieldName form field)
    ]



-- Helper functions for Html


{-| Helper function to build a text input `Html.input [Html.type_ "text", ...] []`
-}
textInput :
    { toMsg : Msg field out -> msg
    , form : Form field err out
    , field : field
    }
    -> Model field err out
    -> Html msg
textInput { toMsg, form, field } model =
    Html.input (HA.type_ "text" :: attrs toMsg form field model) []


{-| Helper function to build a checkbox `Html.input [Html.type_ "checkbox", ...] []`
-}
checkbox :
    { toMsg : Msg field out -> msg
    , form : Form field err out
    , field : field
    , value : String
    }
    -> Model field err out
    -> Html msg
checkbox { toMsg, form, field, value } model =
    Html.input (HA.type_ "checkbox" :: checkedAttrs toMsg form field value model) []


{-| Helper function to build a radio `Html.input [Html.type_ "radio", ...] []`
-}
radio :
    { toMsg : Msg field out -> msg
    , form : Form field err out
    , field : field
    , value : String
    }
    -> Model field err out
    -> Html msg
radio { toMsg, form, field, value } model =
    Html.input (HA.type_ "radio" :: checkedAttrs toMsg form field value model) []


option :
    { form : Form field err out
    , field : field
    , value : String
    , label : String
    }
    -> Model field err out
    -> Html msg
option { form, field, value, label } model =
    Html.option
        (optionAttrs form field value model)
        [ Html.text label ]


select : (Msg field out -> msg) -> Form field err out -> field -> List (Html msg) -> Html msg
select toMsg form field options =
    Html.select (selectAttrs toMsg form field) options
