module Chords.IntegrationSpec exposing (spec)

import Chords
import Chords.Note exposing (..)
import Chords.Types exposing (..)
import Expect
import Test exposing (..)


spec : Test
spec =
    describe "Chords"
        [ describe "parseTab"
            [ test "parses a simple chord" <|
                \_ ->
                    "[Am]"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Parsed (Chord A Minor) ] ]
            , test "parses a simple text" <|
                \_ ->
                    "Hello"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Text "Hello" ] ]
            , test "parses a simple chord with text" <|
                \_ ->
                    "[Am]Hello"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Parsed (Chord A Minor)
                              , Text "Hello"
                              ]
                            ]
            , test "parses consecutive chords" <|
                \_ ->
                    "[Am][E]"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Parsed (Chord A Minor)
                              , Parsed (Chord E Major)
                              ]
                            ]
            , test "parses a more complicated line" <|
                \_ ->
                    "[Am]Hello darkness [C]my old friend[E]"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Parsed (Chord A Minor)
                              , Text "Hello darkness "
                              , Parsed (Chord C Major)
                              , Text "my old friend"
                              , Parsed (Chord E Major)
                              ]
                            ]
            , test "parses multiple lines" <|
                \_ ->
                    "[Am]Hello darkness\n[C]my old friend[E]"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Parsed (Chord A Minor)
                              , Text "Hello darkness"
                              ]
                            , [ Parsed (Chord C Major)
                              , Text "my old friend"
                              , Parsed (Chord E Major)
                              ]
                            ]
            , test "skips empty close brackets" <|
                \_ ->
                    "[Am]Hello[] darkness[]"
                        |> Chords.parseTab
                        |> Expect.equal
                            [ [ Parsed (Chord A Minor)
                              , Text "Hello darkness"
                              ]
                            ]
            ]
        ]
