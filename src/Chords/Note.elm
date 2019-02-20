module Chords.Note exposing
    ( Note(..), Accidental(..)
    , next, transpose, distance
    , toString, toStringWith
    )

{-| Represents the note that gives a name to a chord.

@docs Note, Accidental


# Manipulating Notes

@docs next, transpose, distance


# Exporting

@docs toString, toStringWith

-}


{-| This note is also known as pitch class, because it describes every
occurrence of that note in any octave. Every accidental is described using
the flat notation for consistency.
-}
type Note
    = A
    | Bb
    | B
    | C
    | Db
    | D
    | Eb
    | E
    | F
    | Gb
    | G
    | Ab


{-| This type describes accidentals. Sometimes accidentals can be enharmonic,
which means that they describe the same pitch: one example of this is A Sharp
B Flat.
-}
type Accidental
    = Flat
    | Sharp


{-| Returns the next note.
-}
next : Note -> Note
next note =
    case note of
        A ->
            Bb

        Bb ->
            B

        B ->
            C

        C ->
            Db

        Db ->
            D

        D ->
            Eb

        Eb ->
            E

        E ->
            F

        F ->
            Gb

        Gb ->
            G

        G ->
            Ab

        Ab ->
            A


{-| Returns the note transposed by a number of semitones.
-}
transpose : Int -> Note -> Note
transpose count note =
    case count of
        0 ->
            note

        n ->
            transpose (count - 1) (next note)


{-| Returns the distance in semitones between two notes.
-}
distance : Note -> Note -> Int
distance firstNote secondNote =
    let
        distance_ first second count =
            if transpose count first == second then
                count

            else
                distance_ first second (count + 1)
    in
    distance_ firstNote secondNote 0


{-| Converts a note to String.
-}
toString : Note -> String
toString note =
    toStringWith Sharp note


{-| Converts a note to String. You can decide how to display accidentals.
-}
toStringWith : Accidental -> Note -> String
toStringWith accidental note =
    case note of
        A ->
            "A"

        Bb ->
            case accidental of
                Flat ->
                    "Bb"

                Sharp ->
                    "A#"

        B ->
            "B"

        C ->
            "C"

        Db ->
            case accidental of
                Flat ->
                    "Db"

                Sharp ->
                    "C#"

        D ->
            "D"

        Eb ->
            case accidental of
                Flat ->
                    "Eb"

                Sharp ->
                    "D#"

        E ->
            "E"

        F ->
            "F"

        Gb ->
            case accidental of
                Flat ->
                    "Gb"

                Sharp ->
                    "F#"

        G ->
            "G"

        Ab ->
            case accidental of
                Flat ->
                    "Ab"

                Sharp ->
                    "G#"
