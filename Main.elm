module Main exposing (main)

import Browser
import Chords.Diagram
import Html exposing (Html)


type alias Model =
    List (List (Maybe Int))


initialModel : Model
initialModel =
    [ [ Just 5
      , Just 7
      , Just 7
      , Just 7
      , Just 5
      , Just 5
      ]
    , [ Just 0
      , Just 2
      , Just 2
      , Just 6
      , Just 8
      , Just 0
      ]
    , [ Just 0
      , Just 2
      , Just 2
      , Just 1
      , Just 0
      , Nothing
      ]
    , [ Nothing
      , Just 2
      , Just 2
      , Just 1
      , Nothing
      , Nothing
      ]
    , [ Just 1
      , Just 3
      , Just 4
      , Just 5
      , Just 6
      , Just 0
      ]
    , [ Just 1
      , Just 3
      , Just 4
      , Just 5
      , Just 6
      , Just 1
      ]
    , [ Just 11
      , Just 13
      , Just 13
      , Just 13
      , Just 11
      , Just 11
      ]
    , [ Just 2
      , Just 4
      , Just 5
      , Just 6
      , Just 7
      , Just 2
      ]
    ]


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
    Html.div [] <|
        List.map
            Chords.Diagram.view
            model


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
