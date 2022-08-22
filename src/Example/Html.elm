module Example.Html exposing (main)

import Browser
import Example exposing (..)
import Form.Decoder as Form
import Html exposing (Html)
import Html.Attributes as HA
import Html.Form as Form


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { form : Form.Model ExampleControl String Example
    , submitted : List Example
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        cat =
            animalToString Cat

        zebra =
            animalToString Zebra
    in
    ( { form =
            Form.init Example.emptyExample
                |> Form.setString exampleForm MyText "myText" "hello world"
                |> Form.setChecked exampleForm MyCheckbox "myCheckbox" "y" True
                |> Form.setChecked exampleForm MyRadio "myRadio" zebra True
                |> Form.setString exampleForm MySelect "mySelect" zebra
                |> Form.setChecked exampleForm MyCheckboxes ("myCheckboxes-" ++ cat) cat True
                |> Form.setChecked exampleForm MyCheckboxes ("myCheckboxes-" ++ zebra) zebra True
      , submitted = []
      }
    , Cmd.none
    )


type Msg
    = FormMsg (Form.Msg ExampleControl Example)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg formMsg ->
            Form.update exampleForm formMsg model.form
                |> Form.autoSubmit onSubmit (\f -> { model | form = f })


onSubmit : Model -> Example -> ( Model, Cmd Msg )
onSubmit model ex =
    ( { model | submitted = ex :: List.take 9 model.submitted }
    , Cmd.none
    )


view : Model -> Html Msg
view m =
    Html.article []
        [ Html.h1 [] [ Html.text "Example form" ]
        , viewExampleForm m.form
        , Html.div [] (viewExamples m.submitted)
        ]



-- How to render our controls


viewExampleForm : Form.Model ExampleControl String Example -> Html Msg
viewExampleForm model =
    let
        animals =
            [ Dog, Cat, Zebra ]

        errToText state =
            case state.error of
                Just err ->
                    [ Html.text ("Error: " ++ err) ]

                Nothing ->
                    []

        myTextField =
            Form.textInput
                { toMsg = FormMsg
                , field = MyText
                , control = "myText"
                }

        myCheckboxField =
            Form.checkbox
                { toMsg = FormMsg
                , field = MyCheckbox
                , control = "myCheckbox"
                , value = "y"
                }

        myRadioField animal =
            Form.radio
                { toMsg = FormMsg
                , field = MyRadio
                , control = "myRadio"
                , value = animalToString animal
                }

        myCheckboxesField animal =
            let
                id =
                    animalToString animal
            in
            Form.checkbox
                { toMsg = FormMsg
                , field = MyCheckboxes
                , control = "myCheckboxes-" ++ id
                , value = id
                }
    in
    Html.form (Form.formAttrs FormMsg)
        -- Text
        [ Html.p []
            (Html.label []
                [ Html.text "My text field:"
                , myTextField model
                ]
                :: errToText (Form.fieldState MyText model)
            )

        -- Checkbox
        , Html.p []
            [ Html.label []
                [ myCheckboxField model
                , Html.text "my checkbox"
                ]
            ]

        -- Radio
        , animals
            |> List.map
                (\animal ->
                    Html.label []
                        [ myRadioField animal model
                        , Html.text (animalToString animal)
                        ]
                )
            |> Html.p []

        -- Select
        , animals
            |> List.map animalToString
            |> List.map
                (\id ->
                    Form.option
                        { control = "mySelect"
                        , value = id
                        , label = id
                        }
                        model
                )
            |> Form.select FormMsg MySelect "mySelect"

        -- Checkboxes
        --, Html.p [] (errToText (Form.fieldError "myChec" myCheckboxesErr))
        , animals
            |> List.map
                (\animal ->
                    Html.label []
                        [ myCheckboxesField animal model
                        , Html.text (animalToString animal)
                        ]
                )
            |> Html.p []
        , Html.p [] [ Html.button [ HA.type_ "submit" ] [ Html.text "Submit" ] ]
        ]



-- Form controls


type ExampleControl
    = MyText
    | MyCheckbox
    | MyRadio
    | MySelect
    | MyCheckboxes


exampleForm : ExampleControl -> Form.Field String Example
exampleForm key =
    case key of
        MyText ->
            Form.stringField (\v d -> { d | myText = v })

        --|> Form.assert (Form.minLength "Text field cannot be empty" 1)
        MyCheckbox ->
            Form.boolField (\v d -> { d | myCheckbox = v })

        MyRadio ->
            Form.stringField (\v d -> { d | myRadio = stringToAnimal v })

        MySelect ->
            Form.stringField (\v d -> { d | mySelect = stringToAnimal v })

        MyCheckboxes ->
            Form.checkedField
                (\v d ->
                    { d
                        | myCheckboxes =
                            case stringToAnimal v of
                                Just animal ->
                                    animal :: d.myCheckboxes

                                Nothing ->
                                    d.myCheckboxes
                    }
                )
                (\v d ->
                    { d
                        | myCheckboxes =
                            List.filter
                                (\check -> Just check /= stringToAnimal v)
                                d.myCheckboxes
                    }
                )
