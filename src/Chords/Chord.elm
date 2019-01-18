module Chords.Chord exposing
    ( Chord(..)
    , Quality(..)
    , TertianQuality(..)
    , toString
    )

import Chords.Note as Note exposing (Note)


type TertianQuality
    = Major -- Root, Major Third, Perfect Fifth
    | Minor -- Root, Minor Third, Perfect Fifth
    | Augmented -- Root, Major Third, Augmented Fifth
    | Diminished -- Root, Minor Third, Diminished Fifth
    | Dominant7 -- Root, Major Third, Perfect Fifth, Minor Seventh
    | Major7 -- Root, Major Third, Perfect Fifth, Major Seventh
    | Minor7 -- Root, Minor Third, Perfect Fifth, Major Seventh
    | AugmentedDominant7 -- Root, Major Third, Augmented Fifth, Minor Seventh
    | AugmentedMajor7 -- Root, Major Third, Augmented Fifth, Major Seventh
    | Diminished7 -- Root, Minor Third, Diminished Fifth, Diminished Seventh
    | Major6 -- Root, Major Third, Diminished Fifth, Major Sixth
    | Minor6 -- Root, Minor Third, Diminished Fifth, Major Sixth
    | Dominant9 -- Root, Major Third, Perfect Fifth, Minor Seventh, Major Ninth
    | Major9 -- Root, Major Third, Perfect Fifth, Major Seventh, Major Ninth
    | Minor9 -- Root, Minor Third, Perfect Fifth, Minor Seventh, Major Ninth


type Quality
    = Fifth
    | Tertian TertianQuality
    | Sus2 TertianQuality
    | Sus4 TertianQuality
    | Add9 TertianQuality
    | Add11 TertianQuality
    | NewRoot Note TertianQuality


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

        NewRoot newRoot tertian ->
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

        Dominant9 ->
            "9"

        Major9 ->
            "maj9"

        Minor9 ->
            "m9"
