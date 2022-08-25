module Example.Form exposing (main)

import Browser
import Example exposing (..)
import Form exposing (Form)
import Html exposing (Html)
import Html.Attributes as HA


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
            Form.update formMsg model.form exampleForm
                |> Form.submitOnChange onSubmit (\f -> { model | form = f })


onSubmit : Model -> Example -> ( Model, Cmd Msg )
onSubmit model ex =
    ( { model | submitted = ex :: List.take 9 model.submitted }
    , Cmd.none
    )


view : Model -> Html Msg
view m =
    Html.article []
        [ Html.h1 [] [ Html.text "Example form" ]
        , Html.div [ HA.style "width" "20em" ]
            [ Form.view FormMsg "examples" m.form exampleForm ]
        , Html.div [] (viewExamples m.submitted)
        ]



-- Building our composable form


exampleForm : Form Example
exampleForm =
    let
        animals =
            [ Dog, Cat, Zebra ]

        myTextField =
            Form.textareaField (\v d -> { d | myText = v })
                [ Form.textLabel "myText"
                , Form.default "Hello world"
                ]

        myCheckboxField =
            Form.checkboxField (\v d -> { d | myCheckbox = v })
                [ Form.textLabel "myCheckbox"
                , Form.default True
                ]

        myRadioField =
            Form.radioField
                { set = \v d -> { d | myRadio = v }
                , toString = animalToString
                , options = animals
                , attributes =
                    [ Form.textLabel "myRadio"
                    , Form.default (Just Zebra)
                    , Form.nothingOption "none"
                    ]
                }

        mySelectField =
            Form.selectField
                { set = \v d -> { d | mySelect = v }
                , toString = animalToString
                , options = animals
                , attributes =
                    [ Form.textLabel "mySelect"
                    , Form.nothingOption "Nothing selected"
                    ]
                }

        myCheckboxesField =
            Form.checkboxesField
                { set = \v d -> { d | myCheckboxes = v }
                , toString = animalToString
                , options = animals
                , attributes =
                    [ Form.textLabel "myCheckboxes"
                    , Form.default [ Dog, Zebra ]
                    ]
                }
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
