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
            , test "does not parse an impossible sharp" <|
                \_ ->
                    "E#"
                        |> run
                        |> Expect.err
            , test "does not parse an impossible flat" <|
                \_ ->
                    "Fb"
                        |> run
                        |> Expect.err
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
