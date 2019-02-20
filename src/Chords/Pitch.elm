module Chords.Pitch exposing
    ( Pitch, Octave
    , build
    , next, transpose
    , toNote
    )

{-| Represents a note in a certain octave

@docs Pitch, Octave


# Creating Pitches

@docs build


# Manipulating Pitches

@docs next, transpose


# Exporting

@docs toNote

-}

import Chords.Note as Note exposing (Note(..))


{-| The octave of our pitch.
-}
type alias Octave =
    Int


{-| A Pitch describes a note in a certain octave.
-}
type Pitch
    = Pitch Note Octave


{-| Creates a Pitch.
-}
build : Note -> Octave -> Pitch
build =
    Pitch


{-| Returns the next Pitch, which could be in another octave.
-}
next : Pitch -> Pitch
next (Pitch note octave) =
    let
        ( newNote, newOctave ) =
            case note of
                B ->
                    ( C, octave + 1 )

                other ->
                    ( Note.next other, octave )
    in
    Pitch newNote newOctave


{-| Returns the Pitch transposed by a number of semitones.
-}
transpose : Int -> Pitch -> Pitch
transpose count note =
    case count of
        0 ->
            note

        n ->
            transpose (count - 1) (next note)


{-| Converts a Pitch to a Note.
-}
toNote : Pitch -> Note
toNote (Pitch note _) =
    note
