module Chords.IntegrationSpec exposing (spec)

import Chords
import Expect
import Test exposing (..)


spec : Test
spec =
    describe "Chords"
        [ describe "parseSheet"
            [ test "parses a simple song" <|
                \_ ->
                    simpleSong
                        |> Chords.parseSheet
                        |> Expect.equal []
            ]
        ]


simpleSong : String
simpleSong =
    "[Am]Lascia che sia fio[E7]rito"
