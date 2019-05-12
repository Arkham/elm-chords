module Instruments.Fretted exposing
    ( voicings, voicingInfo
    , matchingNote
    , sortIndex
    )

{-| A representation of a fretted instrument.

@docs voicings, voicingInfo
@docs matchingNote
@docs sortIndex

-}

import Chords exposing (Chord, Voicing)
import Chords.Note as Note exposing (Note)
import Chords.Pitch as Pitch exposing (Pitch)
import List.Extra
import Set


{-| Configure your instrument by passing a tuning and the number of frets.
-}
type alias Config =
    { tuning : List Pitch
    , numFrets : Int
    }


{-| Returns a list of possible voicings of a chord on a fretted instrument.
-}
voicings : Config -> Chord -> List Voicing
voicings { tuning, numFrets } chord =
    let
        numStrings =
            List.length tuning

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


type alias VoicingInfo =
    { allFrets : List Int
    , averageFret : Float
    , fretsAtMaximum : Int
    , fretsAtMinimum : Int
    , fretsCount : Int
    , maximumFret : Int
    , minimumFret : Int
    }


{-| Returns a complete description of the voicing.
-}
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
            allFrets
                |> List.minimum
                |> Maybe.withDefault 0

        maximumFret =
            allFrets
                |> List.maximum
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


{-| Return a tuple that represents the sorting index of the voicing.

We care about:

  - the location of the chord wrt the neck position
  - the span between the lowest fret and the highest fret, considering
  - the average of the frets

-}
sortIndex : Voicing -> Float
sortIndex voicing =
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
            if fretsAtZero >= 1 then
                0

            else
                2

        toIndex a b c =
            toFloat (a * 100) + toFloat (b * 10) + c
    in
    toIndex
        neckWeight
        (maximumFret - minimumFret + zeroFactor + minimumFactor)
        (averageFret / toFloat fretsCount)
