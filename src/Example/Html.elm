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
    ( { form =
            Form.init Example.emptyExample
                |> Form.setString exampleForm MyText "hello world"
                |> Form.setChecked exampleForm MyCheckbox True
                |> Form.setChecked exampleForm (MyRadio Zebra) True
                |> Form.setString exampleForm MySelect (animalToString Zebra)
                |> Form.setChecked exampleForm (MyCheckboxes Zebra) True
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

        myTextState =
            Form.controlState exampleForm MyText model

        myCheckboxesErr =
            Form.fieldErrors exampleForm (List.map MyCheckboxes animals) model
                |> List.head

        errField mErr =
            case mErr of
                Just err ->
                    [ Html.text ("Error: " ++ err) ]

                Nothing ->
                    []
    in
    Html.form (Form.formAttrs FormMsg)
        -- Text
        [ Html.p []
            (Html.label []
                [ Html.text "My text field:"
                , Form.textInput FormMsg exampleForm MyText model
                ]
                :: errField myTextState.error
            )

        -- Checkbox
        , Html.p []
            [ Html.label []
                [ Form.checkbox FormMsg exampleForm MyCheckbox model
                , Html.text "my checkbox"
                ]
            ]

        -- Radio
        , animals
            |> List.map
                (\animal ->
                    Html.label []
                        [ Form.radio FormMsg exampleForm (MyRadio animal) model
                        , Html.text (animalToString animal)
                        ]
                )
            |> Html.p []

        -- Select (no working default values)
        , animals
            |> List.map animalToString
            |> List.map (\id -> Html.option [ HA.value id ] [ Html.text id ])
            |> Form.select FormMsg exampleForm MySelect

        -- Checkboxes
        , Html.p [] (errField myCheckboxesErr)
        , animals
            |> List.map
                (\animal ->
                    Html.label []
                        [ Form.checkbox FormMsg exampleForm (MyCheckboxes animal) model
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
    | MyRadio Animal
    | MySelect
    | MyCheckboxes Animal


exampleForm : ExampleControl -> Form.Control String Example
exampleForm key =
    case key of
        MyText ->
            Form.stringControl
                { name = "myText"
                , validators = [ Form.minLength "Text field cannot be empty" 1 ]
                , update = \v d -> { d | myText = v }
                }

        MyCheckbox ->
            Form.checkedControl
                { name = "myCheckbox"
                , value = "y"
                , validators = []
                , update = \v d -> { d | myCheckbox = v }
                }

        MyRadio animal ->
            Form.checkedControl
                { name = "myRadio"
                , value = animalToString animal
                , validators = []
                , update =
                    Form.onOffUpdate
                        (\d -> { d | myRadio = Just animal })
                        (\d -> { d | myRadio = Nothing })
                }

        MySelect ->
            Form.stringControl
                { name = "mySelect"
                , validators = []
                , update = \v d -> { d | mySelect = stringToAnimal v }
                }

        MyCheckboxes animal ->
            let
                id =
                    animalToString animal
            in
            Form.checkedControl
                { name = "myCheckboxes-" ++ id
                , value = id
                , validators = []
                , update =
                    Form.checkedListUpdate animal
                        .myCheckboxes
                        (\v d -> { d | myCheckboxes = v })
                }
