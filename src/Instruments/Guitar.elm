module Instruments.Guitar exposing
    ( Config
    , Note(..)
    , Voicing
    , defaultTuning
    , voicings
    )

import Chords.Chord as Chord exposing (Chord(..))
import Chords.Note as Note
import List.Extra


type alias Fret =
    Int


type alias Octave =
    Int


{-| An instrument note is a note in a certain octave
-}
type Note
    = Note Note.Note Octave


type alias Tuning =
    List Note


{-| This is the default tuning of a guitar.
-}
defaultTuning : Tuning
defaultTuning =
    [ Note Note.E 2
    , Note Note.A 2
    , Note Note.D 3
    , Note Note.G 3
    , Note Note.B 3
    , Note Note.E 4
    ]


type alias Voicing =
    List (Maybe ( Fret, Note ))


type alias Config =
    { tuning : Tuning
    , numFrets : Int
    }


next : Note -> Note
next (Note note octave) =
    let
        ( newNote, newOctave ) =
            case note of
                Note.B ->
                    ( Note.C, octave + 1 )

                other ->
                    ( Note.next other, octave )
    in
    Note newNote newOctave


transpose : Note -> Int -> Note
transpose note count =
    case count of
        0 ->
            note

        n ->
            transpose (next note) (count - 1)


matchesNote : Note -> Note.Note -> Bool
matchesNote (Note firstNote _) secondNote =
    firstNote == secondNote


voicings : Config -> Chord -> List Voicing
voicings { tuning, numFrets } ((Chord note quality) as chord) =
    let
        desiredNotes =
            Chord.toIntegerNotation chord
                |> List.map (Note.transpose note)

        buildFretNoteRange initial =
            List.range 0 5
                |> List.map (\n -> ( n, transpose initial n ))
                |> List.filter
                    (\( fret, note_ ) ->
                        List.any (matchesNote note_) desiredNotes
                    )

        availableVoicings =
            tuning
                |> List.map buildFretNoteRange
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
            List.all
                (\desired ->
                    List.any
                        (\elem ->
                            case elem of
                                Just ( fret, note_ ) ->
                                    matchesNote note_ desired

                                Nothing ->
                                    False
                        )
                        voicing
                )
                desiredNotes

        sorting : Voicing -> ( Int, Float, Int )
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
            , average - toFloat minimum
            , minimum
            )

        adjustRootNote : Voicing -> Voicing
        adjustRootNote voicing =
            case voicing of
                (Just ( _, first )) :: (Just ( _, second )) :: rest ->
                    if matchesNote first note || matchesNote second note then
                        voicing

                    else
                        Nothing :: Nothing :: rest

                other ->
                    other
    in
    availableVoicings
        |> List.filter hasAllNotes
        |> List.sortBy sorting
        |> List.map adjustRootNote
