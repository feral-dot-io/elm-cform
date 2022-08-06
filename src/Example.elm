module Example exposing (..)

import Html exposing (Html)


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


viewExamples : List Example -> List (Html msg)
viewExamples examples =
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
