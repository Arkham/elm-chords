module Chords.Types exposing
    ( Chord(..)
    , Quality(..)
    , Token(..)
    )

import Chords.Note exposing (Note)


type Quality
    = Major
    | Minor
    | Augmented
    | Diminished


type Chord
    = Chord Note Quality


type Token
    = Text String
    | Parsed Chord
