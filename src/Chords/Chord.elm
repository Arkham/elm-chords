module Chords.Chord exposing
    ( Chord(..)
    , Quality(..)
    , TertianQuality(..)
    , toIntegerNotation
    , toString
    )

import Chords.Note as Note exposing (Note)
import Set


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


toIntegerNotation : Chord -> List Int
toIntegerNotation (Chord root quality) =
    let
        sortAndUnique list =
            list
                |> Set.fromList
                |> Set.toList
                |> List.sort
    in
    case quality of
        Fifth ->
            [ 0, 5 ]

        Tertian tertian ->
            sortAndUnique <| tertianToIntegerNotation tertian

        Sus2 tertian ->
            sortAndUnique <| 2 :: tertianToIntegerNotation tertian

        Sus4 tertian ->
            sortAndUnique <| 5 :: tertianToIntegerNotation tertian

        Add9 tertian ->
            sortAndUnique <| 14 :: tertianToIntegerNotation tertian

        Add11 tertian ->
            sortAndUnique <| 17 :: tertianToIntegerNotation tertian

        NewRoot newRoot tertian ->
            -- TODO
            []


tertianToIntegerNotation : TertianQuality -> List Int
tertianToIntegerNotation tertian =
    case tertian of
        Major ->
            [ 0, 4, 7 ]

        Minor ->
            [ 0, 3, 7 ]

        Augmented ->
            [ 0, 4, 8 ]

        Diminished ->
            [ 0, 3, 6 ]

        Dominant7 ->
            [ 0, 4, 7, 10 ]

        Major7 ->
            [ 0, 4, 7, 11 ]

        Minor7 ->
            [ 0, 3, 7, 10 ]

        AugmentedDominant7 ->
            [ 0, 4, 8, 10 ]

        AugmentedMajor7 ->
            [ 0, 4, 8, 11 ]

        Diminished7 ->
            [ 0, 3, 6, 9 ]

        Major6 ->
            [ 0, 4, 7, 9 ]

        Minor6 ->
            [ 0, 3, 7, 9 ]

        Dominant9 ->
            [ 0, 4, 7, 10, 14 ]

        Major9 ->
            [ 0, 4, 7, 11, 14 ]

        Minor9 ->
            [ 0, 3, 7, 10, 14 ]
