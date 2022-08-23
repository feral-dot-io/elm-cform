module Html.Form exposing
    ( Field
    , Form
    , Model
    , Msg
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
    , submitOnBlur
    , textInput
    , update
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD



-- Types


{-| The Form type holds a conversion function that maps our `control` type to a `Control`
-}
type alias Form field out =
    field -> Field out


{-| Represents a form control. Used to create the events of a page's Html attributes
-}
type Field out
    = Field
        { name : String
        , update : Maybe String -> out -> out
        }



-- Model


{-| Holds our internal DB tracking a form's current values
-}
type Model field out
    = Model (Db field out)


{-| Creates an empty model. Requires an empty value for our returned `out` record. This `out` record is never read and not used in the form's HTML -- fields are only set in response to events. You should use `setString` and setChecked\` to set initial values in the form.
-}
init : out -> Model field out
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
update : Form field out -> Msg field out -> Model field out -> Model field out
update form msg (Model db) =
    case msg of
        OnInput event ->
            Model (updateField form event { db | seenInput = True })

        OnBlur _ ->
            Model { db | seenBlur = True }

        OnSubmit ->
            Model { db | seenSubmit = True }



-- Submission handling


onSubmit :
    (Db field out -> Bool)
    -> (model -> out -> ( model, Cmd msg ))
    -> (Model field out -> model)
    -> Model field out
    -> ( model, Cmd msg )
onSubmit seen next setter model =
    let
        (Model db) =
            model

        model2 =
            resetSeen model
    in
    if seen db then
        next
            (setter (resetSinceSubmit model2))
            (currentOutput model2)

    else
        ( setter model2, Cmd.none )


{-| Processes return values from form updates. Runs a callback whenever any form input changes. For example used on a search query that does something as the user types.
-}
autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model field out -> model) -> Model field out -> ( model, Cmd msg )
autoSubmit =
    onSubmit .seenInput


{-| Similiar to autoSubmit: processes form updates. Updates when a form is submitted. For example the user clicks a `<button type="submit">` or presses enter on a text input.
-}
submitOnBlur : (model -> out -> ( model, Cmd msg )) -> (Model field out -> model) -> Model field out -> ( model, Cmd msg )
submitOnBlur =
    onSubmit .seenBlur


{-| Similiar to autoSubmit: processes form updates. Updates when a form is submitted. For example the user clicks a `<button type="submit">` or presses enter on a text input.
-}
onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model field out -> model) -> Model field out -> ( model, Cmd msg )
onFormSubmit =
    onSubmit .seenSubmit



-- Our internal DB


{-| Holds our internal form state.
-}
type alias Db field out =
    { seenInput : Bool
    , seenBlur : Bool
    , seenSubmit : Bool
    , out : out
    , state : Dict String String
    , touched : List field
    , touchedSinceSubmit : List field
    , changed : List field
    , changedSinceSubmit : List field
    }


emptyDB : out -> Db field out
emptyDB emptyOut =
    Db False False False emptyOut Dict.empty [] [] [] []


{-| Returns the model's current output built up from form events.
-}
currentOutput : Model field out -> out
currentOutput (Model db) =
    db.out


resetSeen : Model field out -> Model field out
resetSeen (Model db) =
    Model
        { db
            | seenInput = False
            , seenBlur = False
            , seenSubmit = False
        }


resetSinceSubmit : Model field out -> Model field out
resetSinceSubmit (Model db) =
    Model { db | touchedSinceSubmit = [], changedSinceSubmit = [] }


updateField : Form field out -> Event field -> Db field out -> Db field out
updateField form event db =
    let
        (Field field) =
            form event.field

        addField s =
            if List.member event.field s then
                s

            else
                event.field :: s
    in
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
        , out = field.update event.value db.out
    }


setValue : Form field out -> field -> Maybe String -> Model field out -> Model field out
setValue form field value (Model db) =
    Model
        (updateField form
            { field = field
            , value = value
            }
            db
        )


setString : Form field out -> field -> String -> Model field out -> Model field out
setString form field value =
    setValue form field (Just value)


setChecked : Form field out -> field -> Bool -> Model field out -> Model field out
setChecked form field checked =
    setValue form
        field
        (if checked then
            Just "y"

         else
            Nothing
        )



-- Creating a control


rawField : (Maybe String -> out -> out) -> String -> Field out
rawField set name =
    Field
        { name = name
        , update = \value out -> set value out
        }


stringField : (String -> out -> out) -> String -> Field out
stringField set =
    rawField (Maybe.withDefault "" >> set)


boolField : (Bool -> out -> out) -> String -> Field out
boolField set =
    rawField (\value out -> set (value /= Nothing) out)


checkedField : (out -> out) -> (out -> out) -> String -> Field out
checkedField on off =
    boolField
        (\checked out ->
            if checked then
                on out

            else
                off out
        )


optionsField : (List option -> out -> out) -> (out -> List option) -> String -> option -> Field out
optionsField set get name opt =
    checkedField
        (\out -> set (opt :: get out) out)
        (\out -> set (List.filter ((/=) opt) (get out)) out)
        name


emptyField : Field out
emptyField =
    rawField (\_ out -> out) ""



-- State handling


type alias FieldState field =
    { field : field
    , value : Maybe String
    , touched : Bool
    , touchedSinceSubmit : Bool
    , changed : Bool
    , changedSinceSubmit : Bool
    }


fieldName : Form field out -> field -> String
fieldName form fieldKey =
    let
        (Field field) =
            form fieldKey
    in
    field.name


fieldValue : Form field out -> field -> Model field out -> Maybe String
fieldValue form field (Model db) =
    Dict.get (fieldName form field) db.state


fieldState : Form field out -> field -> Model field out -> FieldState field
fieldState form field (Model db) =
    { field = field
    , value = fieldValue form field (Model db)
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


attrs : (Msg field out -> msg) -> Form field out -> field -> Model field out -> List (Html.Attribute msg)
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


checkedAttrs : (Msg field out -> msg) -> Form field out -> field -> String -> Model field out -> List (Html.Attribute msg)
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


optionAttrs : Form field out -> field -> String -> Model field out -> List (Html.Attribute msg)
optionAttrs form field value model =
    [ HA.value value
    , HA.selected (fieldValue form field model == Just value)
    ]


selectAttrs : (Msg field out -> msg) -> Form field out -> field -> List (Html.Attribute msg)
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
    , form : Form field out
    , field : field
    }
    -> Model field out
    -> Html msg
textInput { toMsg, form, field } model =
    Html.input (HA.type_ "text" :: attrs toMsg form field model) []


{-| Helper function to build a checkbox `Html.input [Html.type_ "checkbox", ...] []`
-}
checkbox :
    { toMsg : Msg field out -> msg
    , form : Form field out
    , field : field
    , value : String
    }
    -> Model field out
    -> Html msg
checkbox { toMsg, form, field, value } model =
    Html.input (HA.type_ "checkbox" :: checkedAttrs toMsg form field value model) []


{-| Helper function to build a radio `Html.input [Html.type_ "radio", ...] []`
-}
radio :
    { toMsg : Msg field out -> msg
    , form : Form field out
    , field : field
    , value : String
    }
    -> Model field out
    -> Html msg
radio { toMsg, form, field, value } model =
    Html.input (HA.type_ "radio" :: checkedAttrs toMsg form field value model) []


option :
    { form : Form field out
    , field : field
    , value : String
    , label : String
    }
    -> Model field out
    -> Html msg
option { form, field, value, label } model =
    Html.option
        (optionAttrs form field value model)
        [ Html.text label ]


select : (Msg field out -> msg) -> Form field out -> field -> List (Html msg) -> Html msg
select toMsg form field options =
    Html.select (selectAttrs toMsg form field) options
