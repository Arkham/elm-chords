module Chords.Instruments.Voicing exposing
    ( Fret
    , Voicing
    )

import Chords.Instruments.Note exposing (Note)


type alias Fret =
    Int


type alias Voicing =
    List (Maybe ( Fret, Note ))
