module Chords.Instruments.Guitar exposing
    ( Config
    , defaultTuning
    , voicings
    )

import Chords.Chord as Chord exposing (Chord(..))
import Chords.Instruments.Note as Note exposing (Note(..))
import Chords.Instruments.Voicing exposing (Voicing)
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


voicings : Config -> Chord -> List Voicing
voicings { tuning, numFrets } ((Chord note quality) as chord) =
    let
        desiredNotes =
            Chord.toIntegerNotation chord
                |> List.map (RootNote.transpose note)

        fretRange =
            4

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

        sorting : Voicing -> ( Int, Float )
        sorting voicing =
            let
                allFrets =
                    voicing
                        |> List.filterMap identity
                        |> List.map Tuple.first

                minimum =
                    List.minimum allFrets
                        |> Maybe.withDefault 0

                maximum =
                    List.maximum allFrets
                        |> Maybe.withDefault 0

                fretsAtZero =
                    List.Extra.count ((==) 0) allFrets

                fretsAtZeroBonus =
                    if fretsAtZero >= 2 then
                        -fretsAtZero

                    else
                        0

                average =
                    toFloat (List.sum allFrets)
                        / toFloat (List.length allFrets)
            in
            ( maximum - minimum + fretsAtZeroBonus
            , average
                * (average - toFloat minimum)
                / toFloat (List.length allFrets)
            )

        muteNonRoot : Voicing -> Voicing
        muteNonRoot voicing =
            case voicing of
                (Just ( _, first )) :: (Just ( _, second )) :: rest ->
                    if Note.matchesNote first note || Note.matchesNote second note then
                        voicing

                    else
                        Nothing :: Nothing :: rest

                other ->
                    other
    in
    List.range 0 (numFrets - fretRange)
        |> List.concatMap availableVoicings
        |> List.filter hasAllNotes
        |> List.sortBy sorting
        |> List.map muteNonRoot
