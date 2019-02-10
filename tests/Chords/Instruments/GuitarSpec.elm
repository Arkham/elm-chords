module Chords.Instruments.GuitarSpec exposing (spec)

import Chords.Chord exposing (..)
import Chords.Instruments.Guitar as Guitar
import Chords.Instruments.Note as Note
import Chords.Note exposing (Note(..))
import Expect
import Test exposing (..)


spec : Test
spec =
    describe "Instruments.Guitar"
        [ describe "voicings"
            [ test "generates voicings for A minor" <|
                \_ ->
                    Chord A (Tertian Minor)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Just ( 0, Note.Note E 2 )
                                , Just ( 0, Note.Note A 2 )
                                , Just ( 2, Note.Note E 3 )
                                , Just ( 2, Note.Note A 3 )
                                , Just ( 1, Note.Note C 4 )
                                , Just ( 0, Note.Note E 4 )
                                ]
                            )
            , test "generates voicings for D major" <|
                \_ ->
                    Chord D (Tertian Major)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Nothing
                                , Nothing
                                , Just ( 0, Note.Note D 3 )
                                , Just ( 2, Note.Note A 3 )
                                , Just ( 3, Note.Note D 4 )
                                , Just ( 2, Note.Note Gb 4 )
                                ]
                            )
            , test "generates voicings for D minor seventh" <|
                \_ ->
                    Chord D (Tertian Minor7)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Nothing
                                , Nothing
                                , Just ( 0, Note.Note D 3 )
                                , Just ( 2, Note.Note A 3 )
                                , Just ( 1, Note.Note C 4 )
                                , Just ( 1, Note.Note F 4 )
                                ]
                            )
            , test "generates voicings for F major" <|
                \_ ->
                    Chord F (Tertian Major)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Just ( 1, Note.Note F 2 )
                                , Just ( 3, Note.Note C 3 )
                                , Just ( 3, Note.Note F 3 )
                                , Just ( 2, Note.Note A 3 )
                                , Just ( 1, Note.Note C 4 )
                                , Just ( 1, Note.Note F 4 )
                                ]
                            )
            , test "generates voicings for G major" <|
                \_ ->
                    Chord G (Tertian Major)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Just ( 3, Note.Note G 2 )
                                , Just ( 2, Note.Note B 2 )
                                , Just ( 0, Note.Note D 3 )
                                , Just ( 0, Note.Note G 3 )
                                , Just ( 0, Note.Note B 3 )
                                , Just ( 3, Note.Note G 4 )
                                ]
                            )
            , test "generates voicings for B major" <|
                \_ ->
                    Chord B (Tertian Major)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Just ( 2, Note.Note Gb 2 )
                                , Just ( 2, Note.Note B 2 )
                                , Just ( 4, Note.Note Gb 3 )
                                , Just ( 4, Note.Note B 3 )
                                , Just ( 4, Note.Note Eb 4 )
                                , Just ( 2, Note.Note Gb 4 )
                                ]
                            )
            , test "generates voicings for C minor 7" <|
                \_ ->
                    Chord C (Tertian Minor7)
                        |> Guitar.voicings config
                        |> List.head
                        |> Expect.equal
                            (Just
                                [ Just ( 3, Note.Note G 2 )
                                , Just ( 3, Note.Note C 3 )
                                , Just ( 5, Note.Note G 3 )
                                , Just ( 3, Note.Note Bb 3 )
                                , Just ( 4, Note.Note Eb 4 )
                                , Just ( 3, Note.Note G 4 )
                                ]
                            )
            ]
        ]


config : Guitar.Config
config =
    { tuning = Guitar.defaultTuning
    , numFrets = 12
    }
