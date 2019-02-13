module Chords.ChordSpec exposing (spec)

import Chords.Chord as Chord exposing (..)
import Chords.Note as Note exposing (Note(..))
import Expect
import Test exposing (..)


spec : Test
spec =
    describe "Chord"
        [ describe "toIntegerNotation"
            [ test "returns the root note and the integer notation" <|
                \_ ->
                    Chord A (Tertian Major)
                        |> Chord.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 7 ] )
            , test "supports sus2" <|
                \_ ->
                    Chord A (Sus2 Major)
                        |> Chord.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 2, 4, 7 ] )
            , test "supports sus4" <|
                \_ ->
                    Chord A (Sus4 Major)
                        |> Chord.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 5, 7 ] )
            , test "supports add9" <|
                \_ ->
                    Chord A (Add9 Major)
                        |> Chord.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 7, 14 ] )
            , test "supports add11" <|
                \_ ->
                    Chord A (Add11 Major)
                        |> Chord.toIntegerNotation
                        |> Expect.equal ( A, [ 0, 4, 7, 17 ] )
            , test "supports overriding root" <|
                \_ ->
                    Chord C (NewRoot G Major)
                        |> Chord.toIntegerNotation
                        |> Expect.equal ( G, [ 0, 5, 9 ] )
            ]
        ]
