module Instruments.Guitar exposing
    ( Config, Tuning
    , defaultTuning
    , voicings
    )

{-| A guitar representation.

@docs Config, Tuning


# Defaults

@docs defaultTuning


# Find voicings

@docs voicings

-}

import Chords exposing (Chord(..), Voicing)
import Chords.Note as Note exposing (Note(..))
import Chords.Pitch as Pitch exposing (Pitch)
import List.Extra
import Set


{-| The tuning of a guitar corresponds to the pitches of the strings, when
they are played in the open position.
-}
type alias Tuning =
    List Pitch


{-| The default tuning of a guitar.
-}
defaultTuning : Tuning
defaultTuning =
    [ Pitch.build E 2
    , Pitch.build A 2
    , Pitch.build D 3
    , Pitch.build G 3
    , Pitch.build B 3
    , Pitch.build E 4
    ]


{-| Configure your guitar by passing a tuning and the number of frets.
-}
type alias Config =
    { tuning : Tuning
    , numFrets : Int
    }


{-| Returns a list of possible voicings of a chord on a guitar fretboard.
-}
voicings : Config -> Chord -> List Voicing
voicings { tuning, numFrets } chord =
    let
        ( root, integerNotation ) =
            Chords.toIntegerNotation chord

        desiredNotes =
            List.map
                (\offset ->
                    Note.transpose offset root
                )
                integerNotation

        buildFretNoteRange start initial =
            List.range start (start + fretRange)
                |> List.map (\n -> ( n, Pitch.transpose n initial ))
                |> List.filter
                    (\( fret, note_ ) ->
                        List.any (matchingNote note_) desiredNotes
                    )

        availableVoicings n =
            tuning
                |> List.map (buildFretNoteRange n)
                |> List.map
                    (\notes ->
                        if List.isEmpty notes then
                            [ Nothing ]

                        else
                            List.map Just notes
                    )
                |> List.Extra.cartesianProduct
    in
    List.range 0 (numFrets - fretRange)
        |> List.concatMap availableVoicings
        |> List.Extra.uniqueBy Chords.voicingToString
        |> List.filter (hasAllNotes desiredNotes)
        |> List.filter (isErgonomic root)
        |> List.sortBy sorting
        |> List.map (muteNonRoot root)


type alias VoicingInfo =
    { allFrets : List Int
    , averageFret : Float
    , fretsAtMaximum : Int
    , fretsAtMinimum : Int
    , fretsCount : Int
    , maximumFret : Int
    , minimumFret : Int
    }


voicingInfo : Voicing -> VoicingInfo
voicingInfo voicing =
    let
        allFrets =
            voicing
                |> List.filterMap identity
                |> List.map Tuple.first

        fretsCount =
            List.length allFrets

        minimumFret =
            List.minimum allFrets
                |> Maybe.withDefault 0

        maximumFret =
            List.maximum allFrets
                |> Maybe.withDefault 0

        averageFret =
            toFloat (List.sum allFrets)
                / toFloat fretsCount

        fretsAtMinimum =
            List.Extra.count ((==) minimumFret) allFrets

        fretsAtMaximum =
            List.Extra.count ((==) maximumFret) allFrets
    in
    { allFrets = allFrets
    , fretsCount = fretsCount
    , minimumFret = minimumFret
    , maximumFret = maximumFret
    , averageFret = averageFret
    , fretsAtMinimum = fretsAtMinimum
    , fretsAtMaximum = fretsAtMaximum
    }


fretRange : Int
fretRange =
    3


matchingNote : Pitch -> Note -> Bool
matchingNote pitch note =
    pitch
        |> Pitch.toNote
        |> (==) note


hasAllNotes : List Note -> Voicing -> Bool
hasAllNotes desiredNotes voicing =
    voicing
        |> List.filterMap
            (Maybe.map (Tuple.second >> Pitch.toNote >> Note.toString))
        |> Set.fromList
        |> Set.diff
            (Set.fromList (List.map Note.toString desiredNotes))
        |> Set.isEmpty


isErgonomic : Note -> Voicing -> Bool
isErgonomic root voicing =
    let
        { fretsCount, fretsAtMinimum, fretsAtMaximum } =
            voicingInfo voicing

        noHoles =
            fretsCount == List.length voicing

        lowerRoot =
            voicing
                |> List.take 3
                |> List.filterMap identity
                |> List.any
                    (\( _, note ) ->
                        matchingNote note root
                    )
    in
    noHoles && lowerRoot && fretsAtMinimum > 1 && fretsAtMaximum <= 3


sorting : Voicing -> ( Int, Int, Float )
sorting voicing =
    let
        { allFrets, fretsCount, minimumFret, maximumFret, averageFret, fretsAtMinimum, fretsAtMaximum } =
            voicingInfo voicing

        neckWeight =
            round averageFret // (fretRange * 2)

        minimumFactor =
            if fretsAtMinimum >= 3 then
                0

            else
                1

        fretsAtZero =
            List.Extra.count ((==) 0) allFrets

        zeroFactor =
            if fretsAtZero >= 2 then
                0

            else
                2
    in
    ( neckWeight
    , maximumFret - minimumFret + zeroFactor + minimumFactor
    , averageFret / toFloat fretsCount
    )


muteNonRoot : Note -> Voicing -> Voicing
muteNonRoot root voicing =
    case voicing of
        (Just ( _, first )) :: (Just ( _, second )) :: rest ->
            if matchingNote first root || matchingNote second root then
                voicing

            else
                Nothing :: Nothing :: rest

        other ->
            other
