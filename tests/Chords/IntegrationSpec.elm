module Chords.IntegrationSpec exposing (spec)

import Chords
import Chords.Types exposing (..)
import Expect
import Test exposing (..)


spec : Test
spec =
    describe "Chords"
        [ describe "parseSheet"
            [ test "parses a simple chord" <|
                \_ ->
                    "[Am]"
                        |> Chords.parseSheet
                        |> Expect.equal
                            [ [ Parsed (Chord "Am") ] ]
            , test "parses a simple text" <|
                \_ ->
                    "Hello"
                        |> Chords.parseSheet
                        |> Expect.equal
                            [ [ Text "Hello" ] ]
            , test "parses a simple chord with text" <|
                \_ ->
                    "[Am]Hello"
                        |> Chords.parseSheet
                        |> Expect.equal
                            [ [ Parsed (Chord "Am")
                              , Text "Hello"
                              ]
                            ]
            , test "parses a more complicated line" <|
                \_ ->
                    "[Am]Hello darkness [C]my old friend[Em]"
                        |> Chords.parseSheet
                        |> Expect.equal
                            [ [ Parsed (Chord "Am")
                              , Text "Hello darkness "
                              , Parsed (Chord "C")
                              , Text "my old friend"
                              , Parsed (Chord "Em")
                              ]
                            ]
            , test "parses multiple lines" <|
                \_ ->
                    "[Am]Hello darkness\n[C]my old friend[Em]"
                        |> Chords.parseSheet
                        |> Expect.equal
                            [ [ Parsed (Chord "Am")
                              , Text "Hello darkness"
                              ]
                            , [ Parsed (Chord "C")
                              , Text "my old friend"
                              , Parsed (Chord "Em")
                              ]
                            ]
            ]
        ]
