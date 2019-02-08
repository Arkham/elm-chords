module Chords.Note exposing
    ( Note(..)
    , next
    , toString
    , transpose
    )


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


transpose : Note -> Int -> Note
transpose note count =
    case count of
        0 ->
            note

        n ->
            transpose (next note) (count - 1)


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
