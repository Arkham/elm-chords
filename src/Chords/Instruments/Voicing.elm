module Chords.Instruments.Voicing exposing
    ( Fret
    , Voicing
    , toString
    )

import Chords.Instruments.Note exposing (Note)


type alias Fret =
    Int


type alias Voicing =
    List (Maybe ( Fret, Note ))


toString : Voicing -> String
toString voicing =
    voicing
        |> List.map
            (\elem ->
                case elem of
                    Just ( fret, note ) ->
                        String.fromInt fret

                    Nothing ->
                        "X"
            )
        |> String.join ""
