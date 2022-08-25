module Example.Form exposing (main)

import Browser
import Example exposing (..)
import Form exposing (Form)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Input =
    { myText : String
    , myCheckbox : Bool
    , myRadio : Animal
    , mySelect : Maybe Animal
    , myCheckboxes : List Animal
    }


type alias Model =
    { form : Form.Model Input Example
    , submitted : List Example
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form =
            Form.init exampleForm
                emptyExample
                { myText = "Hello world"
                , myCheckbox = True
                , myRadio = Zebra
                , mySelect = Nothing
                , myCheckboxes = [ Dog, Zebra ]
                }
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
        , Form.view FormMsg exampleForm "examples" m.form
        , Html.div [] (viewExamples m.submitted)
        ]



-- Building our composable form


exampleForm : Form Input Example
exampleForm =
    let
        animals =
            [ Dog, Cat, Zebra ]

        myTextField =
            Form.inputField (\v d -> { d | myText = v })
                [ Form.textLabel "myText"
                , Form.default .myText
                ]

        myCheckboxField =
            Form.checkboxField (\v d -> { d | myCheckbox = v })
                [ Form.textLabel "myCheckbox"
                , Form.default .myCheckbox
                ]

        myRadioField =
            Form.radioField
                { set = \v d -> { d | myRadio = v }
                , toString = animalToString
                , options = always animals
                , attributes =
                    [ Form.textLabel "myRadio"
                    , Form.default (.myRadio >> Just)
                    , Form.nothingOption "none"
                    ]
                }

        mySelectField =
            Form.selectField
                { set = \v d -> { d | mySelect = v }
                , toString = animalToString
                , options = always animals
                , attributes =
                    [ Form.textLabel "mySelect"
                    , Form.default .mySelect
                    , Form.nothingOption "Nothing selected"
                    ]
                }

        myCheckboxesField =
            Form.checkboxesField
                { set = \v d -> { d | myCheckboxes = v }
                , toString = animalToString
                , options = always animals
                , attributes =
                    [ Form.textLabel "myCheckboxes"
                    , Form.default .myCheckboxes
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
