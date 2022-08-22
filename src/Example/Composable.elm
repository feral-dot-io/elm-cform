module Example.Composable exposing (main)

import Browser
import Composable.Form as Form exposing (Form)
import Example exposing (..)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { form : Form.Model Example
    , submitted : List Example
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = Form.init emptyExample
      , submitted = []
      }
    , Cmd.none
    )


type Msg
    = FormMsg (Form.Msg Example)


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
        , Form.view FormMsg "examples" exampleForm m.form
        , Html.div [] (viewExamples m.submitted)
        ]



-- Building our composable form


exampleForm : Form Example
exampleForm =
    let
        animals =
            [ Dog, Cat, Zebra ]

        myTextField =
            Form.inputField
                (\v d -> { d | myText = v })
                [ Form.textLabel "myText"
                , Form.default "hello world"
                ]

        myCheckboxField =
            Form.checkboxField (\v d -> { d | myCheckbox = v })
                [ Form.textLabel "myCheckbox"
                , Form.default True
                ]

        myRadioField =
            Form.radiosField (\v d -> { d | myRadio = v })
                animalToString
                animals
                [ Form.textLabel "myRadio"
                , Form.default (Just Zebra)
                ]

        mySelectField =
            Form.selectField (\v d -> { d | mySelect = v })
                animalToString
                stringToAnimal
                animals
                [ Form.textLabel "mySelect"
                , Form.default (Just Zebra)
                ]

        myCheckboxesField =
            Form.checkboxesField
                (\v d -> { d | myCheckboxes = v :: d.myCheckboxes })
                (\v d -> { d | myCheckboxes = List.filter ((/=) v) d.myCheckboxes })
                animalToString
                animals
                [ Form.textLabel "myCheckboxes"
                , Form.default [ Cat, Zebra ]
                ]
    in
    Form.empty
        |> Form.append
            (Form.empty
                |> Form.append myTextField
                |> Form.append myCheckboxField
                |> Form.column
            )
        |> Form.append myRadioField
        |> Form.append mySelectField
        |> Form.append myCheckboxesField
        |> Form.append (Form.submit "Submit")
