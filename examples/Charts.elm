module Charts exposing (main)

import Browser
import Chords exposing (Voicing)
import Chords.Chart
import Html exposing (Html)
import Html.Attributes as Attr
import Instruments.Guitar as Guitar


type alias Model =
    { chords : List String }


initialModel : Model
initialModel =
    { chords =
        [ "Am"
        , "E"
        , "C"
        , "Dm7"
        , "G"
        , "F"
        , "A#"
        , "C#"
        ]
    }


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


view : Model -> Html msg
view model =
    let
        guitar =
            { tuning = Guitar.defaultTuning
            , numFrets = 10
            }

        content =
            model.chords
                |> List.map (\elem -> ( elem, Chords.parseChord elem ))
                |> List.map
                    (\( name, result ) ->
                        case result of
                            Ok chord ->
                                Guitar.voicings guitar chord
                                    |> List.head
                                    |> Maybe.map (viewChart name)
                                    |> Maybe.withDefault
                                        (Html.text ("Could not find voicing for " ++ name))

                            Err err ->
                                Html.text ("Could not parse chord " ++ name)
                    )
    in
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "flexDirection" "row"
        , Attr.style "flexWrap" "wrap"
        ]
        content


viewChart : String -> Voicing -> Html msg
viewChart name voicing =
    Html.div
        [ Attr.style "width" "150px"
        ]
        [ Chords.Chart.view name voicing
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
