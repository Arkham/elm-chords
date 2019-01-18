module Chords.ChordParser exposing (parser)

import Chords.Chord
    exposing
        ( Chord(..)
        , Quality(..)
        , TertianQuality(..)
        )
import Chords.Note exposing (Note(..))
import Parser exposing (..)


parser : Parser Chord
parser =
    let
        buildChord note tertian quality =
            Chord note (quality tertian)
    in
    succeed buildChord
        |= noteParser
        |= oneOf
            [ succeed identity
                |. backtrackable aug
                |= oneOf
                    [ succeed AugmentedDominant7
                        |. symbol "7"
                    , succeed AugmentedMajor7
                        |. maj
                        |. symbol "7"
                    , succeed Augmented
                    ]
            , succeed identity
                |. symbol "d"
                |. symbol "i"
                |. symbol "m"
                |= oneOf
                    [ succeed Diminished7
                        |. symbol "7"
                    , succeed Diminished
                    ]
            , succeed Major7
                |. backtrackable maj
                |. symbol "7"
            , succeed Dominant7
                |. symbol "7"
            , succeed Major6
                |. symbol "6"
            , succeed identity
                |. oneOf
                    [ succeed ()
                        |. symbol "m"
                    , succeed ()
                        |. symbol "-"
                    ]
                |= oneOf
                    [ succeed Minor7
                        |. symbol "7"
                    , succeed Minor6
                        |. symbol "6"
                    , succeed Minor
                    ]
            , succeed Major
            ]
        |= oneOf
            [ succeed (\_ -> Fifth)
                |. symbol "5"
            , succeed identity
                |. symbol "s"
                |. symbol "u"
                |. symbol "s"
                |= oneOf
                    [ succeed Sus2
                        |. symbol "2"
                    , succeed Sus4
                        |. symbol "4"
                    ]
            , succeed identity
                |. symbol "a"
                |. symbol "d"
                |. symbol "d"
                |= oneOf
                    [ succeed Add9
                        |. symbol "9"
                    , succeed Add11
                        |. symbol "1"
                        |. symbol "1"
                    ]
            , succeed NewRoot
                |. symbol "/"
                |= noteParser
            , succeed Tertian
            ]


noteParser : Parser Note
noteParser =
    succeed identity
        |= oneOf
            [ succeed identity
                |. symbol "A"
                |= trio ( Ab, A, Bb )
            , succeed identity
                |. symbol "B"
                |= trio ( Bb, B, C )
            , succeed identity
                |. symbol "C"
                |= trio ( B, C, Db )
            , succeed identity
                |. symbol "D"
                |= trio ( Db, D, Eb )
            , succeed identity
                |. symbol "E"
                |= trio ( Eb, E, F )
            , succeed identity
                |. symbol "F"
                |= trio ( E, F, Gb )
            , succeed identity
                |. symbol "G"
                |= trio ( Gb, G, Ab )
            ]


trio : ( Note, Note, Note ) -> Parser Note
trio ( before, current, after ) =
    succeed identity
        |= oneOf
            [ flatParser before
            , sharpParser after
            , succeed current
            ]


sharpParser : Note -> Parser Note
sharpParser note =
    succeed note
        |. symbol "#"


flatParser : Note -> Parser Note
flatParser note =
    succeed note
        |. symbol "b"


aug : Parser ()
aug =
    succeed ()
        |. oneOf
            [ succeed ()
                |. symbol "a"
                |. symbol "u"
                |. symbol "g"
            , succeed ()
                |. symbol "+"
            ]


maj : Parser ()
maj =
    succeed ()
        |. oneOf
            [ succeed ()
                |. symbol "m"
                |. symbol "a"
                |. symbol "j"
            , succeed ()
                |. symbol "M"
            ]
