module Chords.Instruments.Note exposing
    ( Note(..)
    , matchesNote
    , next
    , transpose
    )

import Chords.Note as RootNote


type alias Octave =
    Int


{-| An instrument note is a note in a certain octave
-}
type Note
    = Note RootNote.Note Octave


next : Note -> Note
next (Note note octave) =
    let
        ( newNote, newOctave ) =
            case note of
                RootNote.B ->
                    ( RootNote.C, octave + 1 )

                other ->
                    ( RootNote.next other, octave )
    in
    Note newNote newOctave


transpose : Note -> Int -> Note
transpose note count =
    case count of
        0 ->
            note

        n ->
            transpose (next note) (count - 1)


matchesNote : Note -> RootNote.Note -> Bool
matchesNote (Note firstNote _) secondNote =
    firstNote == secondNote
