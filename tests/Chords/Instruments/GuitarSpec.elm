module Chords.Instruments.GuitarSpec exposing (spec)

import Chords.Chord exposing (..)
import Chords.Instruments.Guitar as Guitar
import Chords.Instruments.Note as Note
import Chords.Instruments.Voicing as Voicing
import Chords.Note exposing (Note(..))
import Expect
import Test exposing (..)


spec : Test
spec =
    describe "Instruments.Guitar"
        [ describe "voicings"
            [ describe "major chords"
                [ test "A major" <|
                    \_ ->
                        Chord A (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "002220")
                , test "B major" <|
                    \_ ->
                        Chord B (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "224442")
                , test "C major" <|
                    \_ ->
                        Chord C (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "032010")
                , test "D major" <|
                    \_ ->
                        Chord D (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "XX0232")
                , test "E major" <|
                    \_ ->
                        Chord E (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "022100")
                , test "F major" <|
                    \_ ->
                        Chord F (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "133211")
                , test "G major" <|
                    \_ ->
                        Chord G (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "320003")
                ]
            , describe "minor chords"
                [ test "A minor" <|
                    \_ ->
                        Chord A (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "002210")
                , test "B minor" <|
                    \_ ->
                        Chord B (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "224432")
                , test "C minor" <|
                    \_ ->
                        Chord C (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "335543")
                , test "D minor" <|
                    \_ ->
                        Chord D (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "XX0231")
                , test "E minor" <|
                    \_ ->
                        Chord E (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "022000")
                , test "F minor" <|
                    \_ ->
                        Chord F (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "133111")
                , test "G minor" <|
                    \_ ->
                        Chord G (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "310033")
                ]
            , describe "dominant seventh chords"
                [ test "A7" <|
                    \_ ->
                        Chord A (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "002020")
                , test "B7" <|
                    \_ ->
                        Chord B (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "224242")
                , test "C7" <|
                    \_ ->
                        Chord C (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "335353")
                , test "D7" <|
                    \_ ->
                        Chord D (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "XX0212")
                , test "E7" <|
                    \_ ->
                        Chord E (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "020100")
                , test "F7" <|
                    \_ ->
                        Chord F (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "131211")
                , test "G7" <|
                    \_ ->
                        Chord G (Tertian Dominant7)
                            |> firstVoicing
                            |> Expect.equal (Just "320001")
                ]
            , describe "overriding root"
                [ test "C/G" <|
                    \_ ->
                        Chord C (NewRoot G Major)
                            |> firstVoicing
                            |> Expect.equal (Just "332010")
                ]
            ]
        ]


config : Guitar.Config
config =
    { tuning = Guitar.defaultTuning
    , numFrets = 12
    }


firstVoicing : Chord -> Maybe String
firstVoicing chord =
    chord
        |> Guitar.voicings config
        |> List.head
        |> Maybe.map Voicing.toString
