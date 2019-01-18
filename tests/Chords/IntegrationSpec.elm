module Chords.IntegrationSpec exposing (spec)

import Chords
import Chords.Chord as Chord
import Chords.LineParser as LP
import Expect
import Test exposing (..)


{-| This type makes assertions easier
-}
type Token
    = Text String
    | Chord String


spec : Test
spec =
    describe "Chords"
        [ describe "parseTab"
            [ test "parses a simple chord" <|
                \_ ->
                    "[Am]"
                        |> parseTab
                        |> Expect.equal
                            [ [ Chord "Am" ] ]
            , test "parses a simple text" <|
                \_ ->
                    "Hello"
                        |> parseTab
                        |> Expect.equal
                            [ [ Text "Hello" ] ]
            , test "parses a simple chord with text" <|
                \_ ->
                    "[Am]Hello"
                        |> parseTab
                        |> Expect.equal
                            [ [ Chord "Am"
                              , Text "Hello"
                              ]
                            ]
            , test "parses consecutive chords" <|
                \_ ->
                    "[Am][E]"
                        |> parseTab
                        |> Expect.equal
                            [ [ Chord "Am"
                              , Chord "E"
                              ]
                            ]
            , test "parses a more complicated line" <|
                \_ ->
                    "[Am]Hello darkness [C]my old friend[E]"
                        |> parseTab
                        |> Expect.equal
                            [ [ Chord "Am"
                              , Text "Hello darkness "
                              , Chord "C"
                              , Text "my old friend"
                              , Chord "E"
                              ]
                            ]
            , test "parses multiple lines" <|
                \_ ->
                    "[Am]Hello darkness\n[C]my old friend[E]"
                        |> parseTab
                        |> Expect.equal
                            [ [ Chord "Am"
                              , Text "Hello darkness"
                              ]
                            , [ Chord "C"
                              , Text "my old friend"
                              , Chord "E"
                              ]
                            ]
            , test "skips empty close brackets" <|
                \_ ->
                    "[Am]Hello[] darkness[]"
                        |> parseTab
                        |> Expect.equal
                            [ [ Chord "Am"
                              , Text "Hello darkness"
                              ]
                            ]
            ]
        ]


convertToken : LP.Token -> Token
convertToken token =
    case token of
        LP.Text string ->
            Text string

        LP.Parsed chord ->
            Chord <| Chord.toString chord


parseTab : String -> List (List Token)
parseTab tab =
    Chords.parseTab tab
        |> List.map (List.map convertToken)
