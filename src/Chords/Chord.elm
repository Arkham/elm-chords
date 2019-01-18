module Chords.Chord exposing
    ( Chord(..)
    , Quality(..)
    , TertianQuality(..)
    , toString
    )

import Chords.Note as Note exposing (Note)


type TertianQuality
    = Major
    | Minor
    | Augmented
    | Diminished
    | Dominant7
    | Major7
    | Minor7
    | AugmentedDominant7
    | AugmentedMajor7
    | Diminished7
    | Major6
    | Minor6


type Quality
    = Fifth
    | Tertian TertianQuality
    | Sus2 TertianQuality
    | Sus4 TertianQuality
    | Add9 TertianQuality
    | Add11 TertianQuality
    | OverridingRoot Note TertianQuality


type Chord
    = Chord Note Quality


toString : Chord -> String
toString (Chord root quality) =
    case quality of
        Fifth ->
            Note.toString root ++ "5"

        Tertian tertian ->
            Note.toString root
                ++ tertianToString tertian

        Sus2 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "sus2"

        Sus4 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "sus4"

        Add9 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "add9"

        Add11 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "add11"

        OverridingRoot newRoot tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "/"
                ++ Note.toString newRoot


tertianToString : TertianQuality -> String
tertianToString tertian =
    case tertian of
        Major ->
            ""

        Minor ->
            "m"

        Augmented ->
            "+"

        Diminished ->
            "dim"

        Dominant7 ->
            "7"

        Major7 ->
            "maj7"

        Minor7 ->
            "m7"

        AugmentedDominant7 ->
            "+7"

        AugmentedMajor7 ->
            "+M7"

        Diminished7 ->
            "dim7"

        Major6 ->
            "6"

        Minor6 ->
            "m6"
