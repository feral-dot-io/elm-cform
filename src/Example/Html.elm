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
    in
    Html.form (Form.formAttrs FormMsg)
        -- Text
        [ Html.p []
            (Html.label []
                [ Html.text "My text field:"
                , Form.textInput FormMsg exampleForm MyText model
                ]
                :: (case myTextState.error of
                        Just err ->
                            [ Html.text ("Error: " ++ err) ]

                        Nothing ->
                            []
                   )
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
            Form.stringControl (\v d -> { d | myText = v })
                "myText"
                |> Form.withValidator
                    (\str out ->
                        if String.isEmpty str then
                            Err "Text field cannot be empty"

                        else
                            Ok out
                    )

        MyCheckbox ->
            Form.checkedControl
                (\d -> { d | myCheckbox = True })
                (\d -> { d | myCheckbox = False })
                "myCheckbox"
                "y"

        MyRadio animal ->
            Form.checkedControl
                (\d -> { d | myRadio = Just animal })
                (\d -> { d | myRadio = Nothing })
                "myRadio"
                (animalToString animal)

        MySelect ->
            Form.stringControl (\v d -> { d | mySelect = stringToAnimal v })
                "mySelect"

        MyCheckboxes animal ->
            let
                id =
                    animalToString animal
            in
            Form.checkedControl
                (\d -> { d | myCheckboxes = animal :: d.myCheckboxes })
                (\d -> { d | myCheckboxes = List.filter ((/=) animal) d.myCheckboxes })
                ("myCheckboxes-" ++ id)
                id
