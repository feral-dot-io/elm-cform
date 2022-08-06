module Html.Form exposing
    ( Control
    , Form
    , Model
    , Msg
    , SubmitStrategy
    , attrs
    , autoSubmit
    , checkbox
    , checkedControl
    , currentOutput
    , emptyControl
    , formAttrs
    , init
    , onFormSubmit
    , radio
    , select
    , set
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


type alias Form control out =
    control -> Control out


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


type Model out
    = Model (Db out)


init : out -> Model out
init emptyOut =
    Model (emptyDB emptyOut)



-- Update


type Msg control out
    = OnInput control (Maybe String)
    | OnBlur control
    | OnSubmit


update : Form control out -> Msg control out -> Model out -> ( Model out, SubmitStrategy )
update form msg model =
    case msg of
        OnInput ctrl val ->
            ( set form ctrl val model, SubmitOnInput )

        OnBlur _ ->
            ( model, SubmitOnBlur )

        OnSubmit ->
            ( model, SubmitOnForm )



-- Submission handling


type SubmitStrategy
    = SubmitOnInput
    | SubmitOnBlur
    | SubmitOnForm


onSubmit :
    List SubmitStrategy
    -> (model -> out -> ( model, Cmd msg ))
    -> (Model out -> model)
    -> ( Model out, SubmitStrategy )
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


autoSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitStrategy ) -> ( model, Cmd msg )
autoSubmit =
    onSubmit [ SubmitOnInput, SubmitOnForm ]


onFormSubmit : (model -> out -> ( model, Cmd msg )) -> (Model out -> model) -> ( Model out, SubmitStrategy ) -> ( model, Cmd msg )
onFormSubmit =
    onSubmit [ SubmitOnForm ]



-- Our internal DB


type alias Db out =
    { out : out
    , state : Dict String String
    }


emptyDB : out -> Db out
emptyDB emptyOut =
    Db emptyOut Dict.empty


currentOutput : Model out -> out
currentOutput (Model db) =
    db.out


updateDb : (Maybe String -> out -> out) -> String -> Maybe String -> Db out -> Db out
updateDb setter name value db =
    -- TODO: can we merge this in with set?
    { db
        | state =
            case value of
                Just str ->
                    Dict.insert name str db.state

                Nothing ->
                    Dict.remove name db.state
        , out = setter value db.out
    }


set : Form control out -> control -> Maybe String -> Model out -> Model out
set form ctrlKey val (Model db) =
    let
        (Control ctrl) =
            form ctrlKey
    in
    Model (ctrl.update ctrl.name val db)


setString : Form control out -> control -> String -> Model out -> Model out
setString form ctrl val model =
    set form ctrl (Just val) model


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


stringControl : String -> (String -> out -> out) -> Control out
stringControl name setter =
    Control
        { update = updateDb (Maybe.withDefault "" >> setter)
        , name = name
        , value = StringControl
        }


checkedControl : String -> String -> (Bool -> out -> out) -> Control out
checkedControl name value setter =
    Control
        { update = updateDb (\state -> setter (state == Just value))
        , name = name
        , value = CheckedControl value
        }


emptyControl : Control out
emptyControl =
    Control
        { update = \_ _ out -> out
        , name = ""
        , value = StringControl
        }



-- View (HTML attributes)


formAttrs : (Msg control out -> msg) -> List (Html.Attribute msg)
formAttrs toMsg =
    [ HE.onSubmit (toMsg OnSubmit) ]


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


alwaysTargetValueDecoder : JD.Decoder (Maybe String)
alwaysTargetValueDecoder =
    -- Always send target's value
    JD.map Just HE.targetValue


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


attrs : (Msg control out -> msg) -> Form control out -> control -> Model out -> List (Html.Attribute msg)
attrs toMsg form ctrl model =
    eventAttrs toMsg form ctrl ++ stateAttrs form ctrl model


textInput : (Msg control out -> msg) -> Form control out -> control -> Model out -> Html msg
textInput toMsg form control model =
    Html.input (HA.type_ "text" :: attrs toMsg form control model) []


checkbox : (Msg control out -> msg) -> Form control out -> control -> Model out -> Html msg
checkbox toMsg form control model =
    Html.input (HA.type_ "checkbox" :: attrs toMsg form control model) []


radio : (Msg control out -> msg) -> Form control out -> control -> Model out -> Html msg
radio toMsg form control model =
    Html.input (HA.type_ "radio" :: attrs toMsg form control model) []


select : (Msg control out -> msg) -> Form control out -> control -> List (Html msg) -> Html msg
select toMsg form control options =
    Html.select (eventAttrs toMsg form control) options
