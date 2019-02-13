module Chords.Instruments.Guitar exposing
    ( Config
    , defaultTuning
    , voicings
    )

import Chords.Chord as Chord exposing (Chord(..))
import Chords.Instruments.Note as Note exposing (Note(..))
import Chords.Instruments.Voicing as Voicing exposing (Voicing)
import Chords.Note as RootNote
import List.Extra
import Set


type alias Tuning =
    List Note


{-| This is the default tuning of a guitar.
-}
defaultTuning : Tuning
defaultTuning =
    [ Note RootNote.E 2
    , Note RootNote.A 2
    , Note RootNote.D 3
    , Note RootNote.G 3
    , Note RootNote.B 3
    , Note RootNote.E 4
    ]


type alias Config =
    { tuning : Tuning
    , numFrets : Int
    }


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


voicings : Config -> Chord -> List Voicing
voicings { tuning, numFrets } chord =
    let
        ( root, integerNotation ) =
            Chord.toIntegerNotation chord

        desiredNotes =
            List.map (RootNote.transpose root)
                integerNotation

        fretRange =
            3

        buildFretNoteRange start initial =
            List.range start (start + fretRange)
                |> List.map (\n -> ( n, Note.transpose initial n ))
                |> List.filter
                    (\( fret, note_ ) ->
                        List.any (Note.matchesNote note_) desiredNotes
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

        hasAllNotes : Voicing -> Bool
        hasAllNotes voicing =
            voicing
                |> List.filterMap
                    (Maybe.map (\( fret, Note n _ ) -> RootNote.toString n))
                |> Set.fromList
                |> Set.diff
                    (Set.fromList (List.map RootNote.toString desiredNotes))
                |> Set.isEmpty

        isErgonomic : Voicing -> Bool
        isErgonomic voicing =
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
                                Note.matchesNote note root
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

        muteNonRoot : Voicing -> Voicing
        muteNonRoot voicing =
            case voicing of
                (Just ( _, first )) :: (Just ( _, second )) :: rest ->
                    if Note.matchesNote first root || Note.matchesNote second root then
                        voicing

                    else
                        Nothing :: Nothing :: rest

                other ->
                    other
    in
    List.range 0 (numFrets - fretRange)
        |> List.concatMap availableVoicings
        |> List.Extra.uniqueBy Voicing.toString
        |> List.filter hasAllNotes
        |> List.filter isErgonomic
        |> List.sortBy sorting
        |> List.map muteNonRoot
