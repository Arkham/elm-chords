module Chords.ChordParserSpec exposing (spec)

import Chords.ChordParser as ChordParser
import Chords.Note exposing (..)
import Chords.Types exposing (..)
import Expect
import Parser as P exposing ((|.), (|=))
import Test exposing (..)


spec : Test
spec =
    describe "ChordsParser"
        [ describe "parse"
            [ test "parses a major chord" <|
                \_ ->
                    "C"
                        |> run
                        |> Expect.equal (Ok (Chord C Major))
            , test "parses a sharp chord" <|
                \_ ->
                    "C#"
                        |> run
                        |> Expect.equal (Ok (Chord Db Major))
            , test "parses a flat chord" <|
                \_ ->
                    "Bb"
                        |> run
                        |> Expect.equal (Ok (Chord Bb Major))
            , test "parses a sharp chord equivalent" <|
                \_ ->
                    "E#"
                        |> run
                        |> Expect.equal (Ok (Chord F Major))
            , test "parses a flat chord equivalent" <|
                \_ ->
                    "Fb"
                        |> run
                        |> Expect.equal (Ok (Chord E Major))
            , test "parses a minor chord" <|
                \_ ->
                    "Am"
                        |> run
                        |> Expect.equal (Ok (Chord A Minor))
            , test "parses a minor chord with a dash" <|
                \_ ->
                    "A-"
                        |> run
                        |> Expect.equal (Ok (Chord A Minor))
            , test "parses an augmented chord" <|
                \_ ->
                    "Aaug"
                        |> run
                        |> Expect.equal (Ok (Chord A Augmented))
            , test "parses a diminished chord" <|
                \_ ->
                    "Adim"
                        |> run
                        |> Expect.equal (Ok (Chord A Diminished))
            , test "parses a dominant seventh" <|
                \_ ->
                    "A7"
                        |> run
                        |> Expect.equal (Ok (Chord A Dominant7))
            , test "parses a major seventh" <|
                \_ ->
                    "Amaj7"
                        |> run
                        |> Expect.equal (Ok (Chord A Major7))
            , test "parses a minor seventh" <|
                \_ ->
                    "Am7"
                        |> run
                        |> Expect.equal (Ok (Chord A Minor7))
            , test "parses an augmented seventh" <|
                \_ ->
                    "Aaug7"
                        |> run
                        |> Expect.equal (Ok (Chord A Augmented7))
            , test "parses a diminished seventh" <|
                \_ ->
                    "Adim7"
                        |> run
                        |> Expect.equal (Ok (Chord A Diminished7))
            , test "parses a power chord" <|
                \_ ->
                    "A5"
                        |> run
                        |> Expect.equal (Ok (Chord A Fifth))
            , test "parses a major sixth" <|
                \_ ->
                    "D6"
                        |> run
                        |> Expect.equal (Ok (Chord D Major6))
            , test "parses a minor sixth" <|
                \_ ->
                    "Dm6"
                        |> run
                        |> Expect.equal (Ok (Chord D Minor6))
            ]
        ]


run : String -> Result (List P.DeadEnd) Chord
run string =
    let
        -- in this test we want to match the whole string
        testParser : P.Parser Chord
        testParser =
            P.succeed identity
                |= ChordParser.parser
                |. P.end
    in
    P.run testParser string
