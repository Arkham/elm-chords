module Chords.Note exposing (Note(..), toString)


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


type Accidental
    = Flat
    | Sharp


toString : Note -> String
toString note =
    toStringWith Sharp note


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
                    "G#"

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
