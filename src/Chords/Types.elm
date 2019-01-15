module Chords.Types exposing
    ( Chord(..)
    , Token(..)
    )


type Chord
    = Chord String


type Token
    = Text String
    | Parsed Chord
