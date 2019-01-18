module Chords.ChordParserSpec exposing (spec)

import Chords.Chord as Chord exposing (Chord)
import Chords.ChordParser as ChordParser
import Expect
import Parser exposing ((|.), (|=))
import Test exposing (..)


spec : Test
spec =
    describe "ChordsParser"
        [ describe "parse"
            [ test "parses a major chord" <|
                \_ ->
                    "C"
                        |> parse
                        |> Expect.equal (Ok "C")
            , test "parses a sharp chord" <|
                \_ ->
                    "C#"
                        |> parse
                        |> Expect.equal (Ok "C#")
            , test "parses a flat chord" <|
                \_ ->
                    "Bb"
                        |> parse
                        |> Expect.equal (Ok "A#")
            , test "parses a sharp chord equivalent" <|
                \_ ->
                    "E#"
                        |> parse
                        |> Expect.equal (Ok "F")
            , test "parses a flat chord equivalent" <|
                \_ ->
                    "Fb"
                        |> parse
                        |> Expect.equal (Ok "E")
            , test "parses a minor chord" <|
                \_ ->
                    "Am"
                        |> parse
                        |> Expect.equal (Ok "Am")
            , test "parses a minor chord with a dash" <|
                \_ ->
                    "A-"
                        |> parse
                        |> Expect.equal (Ok "Am")
            , test "parses an augmented chord" <|
                \_ ->
                    "Aaug"
                        |> parse
                        |> Expect.equal (Ok "A+")
            , test "parses an augmented chord using plus notation" <|
                \_ ->
                    "A+"
                        |> parse
                        |> Expect.equal (Ok "A+")
            , test "parses a diminished chord" <|
                \_ ->
                    "Adim"
                        |> parse
                        |> Expect.equal (Ok "Adim")
            , test "parses a dominant seventh" <|
                \_ ->
                    "A7"
                        |> parse
                        |> Expect.equal (Ok "A7")
            , test "parses a major seventh" <|
                \_ ->
                    "Amaj7"
                        |> parse
                        |> Expect.equal (Ok "Amaj7")
            , test "parses a minor seventh" <|
                \_ ->
                    "Am7"
                        |> parse
                        |> Expect.equal (Ok "Am7")
            , test "parses an augmented dominant seventh" <|
                \_ ->
                    "Aaug7"
                        |> parse
                        |> Expect.equal (Ok "A+7")
            , test "parses an augmented major seventh" <|
                \_ ->
                    "Aaugmaj7"
                        |> parse
                        |> Expect.equal (Ok "A+M7")
            , test "parses a diminished seventh" <|
                \_ ->
                    "Adim7"
                        |> parse
                        |> Expect.equal (Ok "Adim7")
            , test "parses a power chord" <|
                \_ ->
                    "A5"
                        |> parse
                        |> Expect.equal (Ok "A5")
            , test "parses a major sixth" <|
                \_ ->
                    "D6"
                        |> parse
                        |> Expect.equal (Ok "D6")
            , test "parses a minor sixth" <|
                \_ ->
                    "Dm6"
                        |> parse
                        |> Expect.equal (Ok "Dm6")
            , test "parses a dominant ninth" <|
                \_ ->
                    "D9"
                        |> parse
                        |> Expect.equal (Ok "D9")
            , test "parses a major ninth" <|
                \_ ->
                    "Dmaj9"
                        |> parse
                        |> Expect.equal (Ok "Dmaj9")
            , test "parses a minor ninth" <|
                \_ ->
                    "Dm9"
                        |> parse
                        |> Expect.equal (Ok "Dm9")
            , test "parses a suspended second" <|
                \_ ->
                    "Dsus2"
                        |> parse
                        |> Expect.equal (Ok "Dsus2")
            , test "parses a suspended fourth" <|
                \_ ->
                    "Dsus4"
                        |> parse
                        |> Expect.equal (Ok "Dsus4")
            , test "parses an added ninth" <|
                \_ ->
                    "Dadd9"
                        |> parse
                        |> Expect.equal (Ok "Dadd9")
            , test "parses an added eleventh" <|
                \_ ->
                    "Dadd11"
                        |> parse
                        |> Expect.equal (Ok "Dadd11")
            , test "parses a chord with overridden root" <|
                \_ ->
                    "C7/F"
                        |> parse
                        |> Expect.equal (Ok "C7/F")
            ]
        ]


parse : String -> Result (List Parser.DeadEnd) String
parse string =
    let
        -- in this test we want to match the whole string
        testParser : Parser.Parser Chord
        testParser =
            Parser.succeed identity
                |= ChordParser.parser
                |. Parser.end
    in
    string
        |> Parser.run testParser
        |> Result.map Chord.toString
