module Example exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Form as Form


type alias Example =
    { myText : String
    , myCheckbox : Bool
    , myRadio : Maybe Animal
    , mySelect : Maybe Animal
    , myCheckboxes : List Animal
    }


type Animal
    = Cat
    | Dog
    | Zebra


emptyExample : Example
emptyExample =
    Example "" False Nothing Nothing []


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
    ( { form =
            Form.init emptyExample
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
        , Html.div [] (viewSubmitted m.submitted)
        ]


viewExampleForm : Form.Model Example -> Html Msg
viewExampleForm model =
    let
        animals =
            [ Dog, Cat, Zebra ]
    in
    Html.form (Form.formAttrs FormMsg)
        -- Text
        [ Html.p []
            [ Html.label []
                [ Html.text "My text field:"
                , Form.textInput FormMsg exampleForm MyText model
                ]
            ]

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


viewSubmitted : List Example -> List (Html Msg)
viewSubmitted examples =
    if List.isEmpty examples then
        []

    else
        [ Html.h2 [] [ Html.text "Submitted" ]
        , Html.table []
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "text" ]
                    , Html.th [] [ Html.text "checkbox" ]
                    , Html.th [] [ Html.text "radio" ]
                    , Html.th [] [ Html.text "select" ]
                    , Html.th [] [ Html.text "checkboxes" ]
                    ]
                ]
            , examples
                |> List.map
                    (\ex ->
                        let
                            text t =
                                if String.isEmpty t then
                                    Html.em [] [ Html.text "empty" ]

                                else
                                    Html.text t

                            mText =
                                Maybe.withDefault "" >> text
                        in
                        Html.tr []
                            [ Html.td [] [ text ex.myText ]
                            , Html.td []
                                [ Html.text
                                    (if ex.myCheckbox then
                                        "checked"

                                     else
                                        "unchecked"
                                    )
                                ]
                            , Html.td [] [ mText (Maybe.map animalToString ex.myRadio) ]
                            , Html.td [] [ mText (Maybe.map animalToString ex.mySelect) ]
                            , Html.td []
                                [ Html.text
                                    (ex.myCheckboxes
                                        |> List.map animalToString
                                        |> String.join ", "
                                    )
                                ]
                            ]
                    )
                |> Html.tbody []
            ]
        ]


animalToString : Animal -> String
animalToString a =
    case a of
        Cat ->
            "cat"

        Dog ->
            "dog"

        Zebra ->
            "zebra"


stringToAnimal : String -> Maybe Animal
stringToAnimal str =
    case str of
        "cat" ->
            Just Cat

        "dog" ->
            Just Dog

        "zebra" ->
            Just Zebra

        _ ->
            Nothing


type ExampleControl
    = MyText
    | MyCheckbox
    | MyRadio Animal
    | MySelect
    | MyCheckboxes Animal


exampleForm : ExampleControl -> Form.Control Example
exampleForm key =
    case key of
        MyText ->
            Form.stringControl "myText"
                (\v d -> { d | myText = v })

        MyCheckbox ->
            Form.checkedControl "myCheckbox"
                "y"
                (\v d -> { d | myCheckbox = v })

        MyRadio animal ->
            Form.checkedControl
                "myRadio"
                (animalToString animal)
                (\v d ->
                    { d
                        | myRadio =
                            if v then
                                Just animal

                            else
                                Nothing
                    }
                )

        MySelect ->
            Form.stringControl "mySelect"
                (\v d -> { d | mySelect = stringToAnimal v })

        MyCheckboxes animal ->
            let
                id =
                    animalToString animal
            in
            Form.checkedControl ("myCheckboxes-" ++ id)
                id
                (\v d ->
                    { d
                        | myCheckboxes =
                            if v then
                                animal :: d.myCheckboxes

                            else
                                List.filter ((/=) animal) d.myCheckboxes
                    }
                )
