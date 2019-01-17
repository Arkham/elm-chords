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
    | Dominant7
    | Major7
    | Minor7
    | Augmented7
    | Diminished7
    | Fifth
    | Major6
    | Minor6


type Chord
    = Chord Note Quality


type Token
    = Text String
    | Parsed Chord
