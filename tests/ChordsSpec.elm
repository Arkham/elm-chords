module ChordsSpec exposing (spec)

import Chords exposing (Chord(..), Quality(..), TertianQuality(..))
import Chords.Note exposing (Note(..))
import Expect
import Parser
import Test exposing (..)


spec : Test
spec =
    describe "Chords"
        [ describe "toIntegerNotation"
            [ test "returns the root note and the integer notation" <|
                \_ ->
                    Chord A (Tertian Major)
                        |> Chords.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 7 ] )
            , test "supports sus2" <|
                \_ ->
                    Chord A (Sus2 Major)
                        |> Chords.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 2, 4, 7 ] )
            , test "supports sus4" <|
                \_ ->
                    Chord A (Sus4 Major)
                        |> Chords.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 5, 7 ] )
            , test "supports add9" <|
                \_ ->
                    Chord A (Add9 Major)
                        |> Chords.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 7, 14 ] )
            , test "supports add11" <|
                \_ ->
                    Chord A (Add11 Major)
                        |> Chords.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 7, 17 ] )
            , test "supports overriding root" <|
                \_ ->
                    Chord C (NewRoot G Major)
                        |> Chords.toIntegerNotation
                        |> Expect.equal ( G, [ 0, 5, 9 ] )
            ]
        , describe "parseChord"
            [ test "parses a major chord" <|
                \_ ->
                    "C"
                        |> parseChord
                        |> Expect.equal (Ok "C")
            , test "parses a sharp chord" <|
                \_ ->
                    "C#"
                        |> parseChord
                        |> Expect.equal (Ok "C#")
            , test "parses a flat chord" <|
                \_ ->
                    "Bb"
                        |> parseChord
                        |> Expect.equal (Ok "A#")
            , test "parses a sharp chord equivalent" <|
                \_ ->
                    "E#"
                        |> parseChord
                        |> Expect.equal (Ok "F")
            , test "parses a flat chord equivalent" <|
                \_ ->
                    "Fb"
                        |> parseChord
                        |> Expect.equal (Ok "E")
            , test "parses a minor chord" <|
                \_ ->
                    "Am"
                        |> parseChord
                        |> Expect.equal (Ok "Am")
            , test "parses a minor chord with a dash" <|
                \_ ->
                    "A-"
                        |> parseChord
                        |> Expect.equal (Ok "Am")
            , test "parses an augmented chord" <|
                \_ ->
                    "Aaug"
                        |> parseChord
                        |> Expect.equal (Ok "A+")
            , test "parses an augmented chord using plus notation" <|
                \_ ->
                    "A+"
                        |> parseChord
                        |> Expect.equal (Ok "A+")
            , test "parses a diminished chord" <|
                \_ ->
                    "Adim"
                        |> parseChord
                        |> Expect.equal (Ok "Adim")
            , test "parses a dominant seventh" <|
                \_ ->
                    "A7"
                        |> parseChord
                        |> Expect.equal (Ok "A7")
            , test "parses a major seventh" <|
                \_ ->
                    "Amaj7"
                        |> parseChord
                        |> Expect.equal (Ok "Amaj7")
            , test "parses a minor seventh" <|
                \_ ->
                    "Am7"
                        |> parseChord
                        |> Expect.equal (Ok "Am7")
            , test "parses an augmented dominant seventh" <|
                \_ ->
                    "Aaug7"
                        |> parseChord
                        |> Expect.equal (Ok "A+7")
            , test "parses an augmented major seventh" <|
                \_ ->
                    "Aaugmaj7"
                        |> parseChord
                        |> Expect.equal (Ok "A+M7")
            , test "parses a diminished seventh" <|
                \_ ->
                    "Adim7"
                        |> parseChord
                        |> Expect.equal (Ok "Adim7")
            , test "parses a power chord" <|
                \_ ->
                    "A5"
                        |> parseChord
                        |> Expect.equal (Ok "A5")
            , test "parses a major sixth" <|
                \_ ->
                    "D6"
                        |> parseChord
                        |> Expect.equal (Ok "D6")
            , test "parses a minor sixth" <|
                \_ ->
                    "Dm6"
                        |> parseChord
                        |> Expect.equal (Ok "Dm6")
            , test "parses a dominant ninth" <|
                \_ ->
                    "D9"
                        |> parseChord
                        |> Expect.equal (Ok "D9")
            , test "parses a major ninth" <|
                \_ ->
                    "Dmaj9"
                        |> parseChord
                        |> Expect.equal (Ok "Dmaj9")
            , test "parses a minor ninth" <|
                \_ ->
                    "Dm9"
                        |> parseChord
                        |> Expect.equal (Ok "Dm9")
            , test "parses a suspended second" <|
                \_ ->
                    "Dsus2"
                        |> parseChord
                        |> Expect.equal (Ok "Dsus2")
            , test "parses a seventh chord with suspended second" <|
                \_ ->
                    "D7sus2"
                        |> parseChord
                        |> Expect.equal (Ok "D7sus2")
            , test "parses a suspended fourth" <|
                \_ ->
                    "Dsus4"
                        |> parseChord
                        |> Expect.equal (Ok "Dsus4")
            , test "parses an added ninth" <|
                \_ ->
                    "Dadd9"
                        |> parseChord
                        |> Expect.equal (Ok "Dadd9")
            , test "parses an added eleventh" <|
                \_ ->
                    "Dadd11"
                        |> parseChord
                        |> Expect.equal (Ok "Dadd11")
            , test "parses a chord with overridden root" <|
                \_ ->
                    "C7/F"
                        |> parseChord
                        |> Expect.equal (Ok "C7/F")
            ]
        , describe "parseSheet"
            [ test "parses a simple chord" <|
                \_ ->
                    "[Am]"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Parsed "Am" ] ]
            , test "parses simple lyrics" <|
                \_ ->
                    "Hello"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Lyrics "Hello" ] ]
            , test "parses a simple chord with lyrics" <|
                \_ ->
                    "[Am]Hello"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Parsed "Am"
                              , Lyrics "Hello"
                              ]
                            ]
            , test "parses consecutive chords" <|
                \_ ->
                    "[Am][E]"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Parsed "Am"
                              , Parsed "E"
                              ]
                            ]
            , test "parses a more complicated line" <|
                \_ ->
                    "[Am]Hello darkness [C]my old friend[E]"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Parsed "Am"
                              , Lyrics "Hello darkness "
                              , Parsed "C"
                              , Lyrics "my old friend"
                              , Parsed "E"
                              ]
                            ]
            , test "parses multiple lines" <|
                \_ ->
                    "[Am]Hello darkness\n[C]my old friend[E]"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Parsed "Am"
                              , Lyrics "Hello darkness"
                              ]
                            , [ Parsed "C"
                              , Lyrics "my old friend"
                              , Parsed "E"
                              ]
                            ]
            , test "skips empty close brackets" <|
                \_ ->
                    "[Am]Hello[] darkness[]"
                        |> parseSheet
                        |> Expect.equal
                            [ [ Parsed "Am"
                              , Lyrics "Hello darkness"
                              ]
                            ]
            ]
        ]


parseChord : String -> Result (List Parser.DeadEnd) String
parseChord string =
    Chords.parseChord string
        |> Result.map Chords.toString


type Token
    = Lyrics String
    | Parsed String


convertToken : Chords.Token -> Token
convertToken token =
    case token of
        Chords.Lyrics string ->
            Lyrics string

        Chords.Parsed chord ->
            Parsed <| Chords.toString chord


parseSheet : String -> List (List Token)
parseSheet sheet =
    sheet
        |> Chords.parseSheet
        |> List.map
            (\result ->
                case result of
                    Ok tokens ->
                        List.map convertToken tokens

                    Err _ ->
                        []
            )
