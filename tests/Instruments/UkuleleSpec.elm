module Instruments.UkuleleSpec exposing (spec)

import Chords exposing (..)
import Chords.Note exposing (Note(..))
import Expect
import Instruments.Ukulele as Ukulele
import Test exposing (..)


spec : Test
spec =
    describe "Instruments.Ukulele"
        [ describe "voicings"
            [ describe "major chords"
                [ test "A major" <|
                    \_ ->
                        Chord A (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "2100")
                , test "C major" <|
                    \_ ->
                        Chord C (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "0003")
                , test "G major" <|
                    \_ ->
                        Chord G (Tertian Major)
                            |> firstVoicing
                            |> Expect.equal (Just "0232")
                ]
            , describe "minor chords"
                [ test "A minor" <|
                    \_ ->
                        Chord A (Tertian Minor)
                            |> firstVoicing
                            |> Expect.equal (Just "2000")
                ]
            ]
        ]


config : Ukulele.Config
config =
    { tuning = Ukulele.defaultTuning
    , numFrets = 12
    }


firstVoicing : Chord -> Maybe String
firstVoicing chord =
    chord
        |> Ukulele.voicings config
        |> List.head
        |> Maybe.map voicingToString
