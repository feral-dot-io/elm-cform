module Example.Html exposing (main)

import Browser
import Example exposing (..)
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
    { form : Form.Model ExampleControl Example
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
                |> Form.setString exampleForm MyText "hello world"
                |> Form.setChecked exampleForm MyCheckbox True
                |> Form.setString exampleForm MyRadio zebra
                |> Form.setString exampleForm MySelect zebra
                |> Form.setString exampleForm (MyCheckboxes Cat) cat
                |> Form.setString exampleForm (MyCheckboxes Zebra) zebra
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


viewExampleForm : Form.Model ExampleControl Example -> Html Msg
viewExampleForm model =
    let
        animals =
            [ Dog, Cat, Zebra ]

        myTextField =
            Form.textInput
                { toMsg = FormMsg
                , form = exampleForm
                , field = MyText
                }

        myCheckboxField =
            Form.checkbox
                { toMsg = FormMsg
                , form = exampleForm
                , field = MyCheckbox
                , value = "y"
                }

        myRadioField animal =
            Form.radio
                { toMsg = FormMsg
                , form = exampleForm
                , field = MyRadio
                , value = animalToString animal
                }

        myCheckboxesField animal =
            let
                id =
                    animalToString animal
            in
            Form.checkbox
                { toMsg = FormMsg
                , form = exampleForm
                , field = MyCheckboxes animal
                , value = id
                }
    in
    Html.form (Form.formAttrs FormMsg)
        -- Text
        [ Html.p []
            [ Html.label []
                [ Html.text "My text field:"
                , myTextField model
                ]
            ]

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
                        { form = exampleForm
                        , field = MySelect
                        , value = id
                        , label = id
                        }
                        model
                )
            |> Form.select FormMsg exampleForm MySelect

        -- Checkboxes
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
    | MyCheckboxes Animal


exampleForm : ExampleControl -> Form.Field Example
exampleForm key =
    case key of
        MyText ->
            Form.stringField (\v d -> { d | myText = v })
                "myText"

        MyCheckbox ->
            Form.boolField (\v d -> { d | myCheckbox = v })
                "myCheckbox"

        MyRadio ->
            Form.stringField (\v d -> { d | myRadio = stringToAnimal v })
                "myRadio"

        MySelect ->
            Form.stringField (\v d -> { d | mySelect = stringToAnimal v })
                "mySelect"

        MyCheckboxes animal ->
            Form.optionsField
                (\v d -> { d | myCheckboxes = v })
                .myCheckboxes
                ("myCheckboxes" ++ animalToString animal)
                animal
