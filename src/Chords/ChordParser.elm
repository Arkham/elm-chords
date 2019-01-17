module Chords.ChordParser exposing (parser)

import Chords.Note exposing (Note(..))
import Chords.Types exposing (Chord(..), Quality(..))
import Parser exposing (..)


parser : Parser Chord
parser =
    succeed Chord
        |= noteParser
        |= oneOf
            [ succeed Minor
                |. symbol "m"
            , succeed Minor
                |. symbol "-"
            , succeed Major
            ]


noteParser : Parser Note
noteParser =
    succeed identity
        |= oneOf
            [ succeed identity
                |. symbol "A"
                |= oneOf
                    [ sharpParser Bb
                    , flatParser Ab
                    , succeed A
                    ]
            , succeed identity
                |. symbol "B"
                |= oneOf
                    [ flatParser Bb
                    , succeed B
                    ]
            , succeed identity
                |. symbol "C"
                |= oneOf
                    [ sharpParser Db
                    , succeed C
                    ]
            , succeed identity
                |. symbol "D"
                |= oneOf
                    [ sharpParser Eb
                    , flatParser Db
                    , succeed D
                    ]
            , succeed identity
                |. symbol "E"
                |= oneOf
                    [ flatParser Eb
                    , succeed E
                    ]
            , succeed identity
                |. symbol "F"
                |= oneOf
                    [ sharpParser Gb
                    , succeed F
                    ]
            , succeed identity
                |. symbol "G"
                |= oneOf
                    [ sharpParser Ab
                    , flatParser Gb
                    , succeed G
                    ]
            ]


sharpParser : Note -> Parser Note
sharpParser note =
    succeed note
        |. symbol "#"


flatParser : Note -> Parser Note
flatParser note =
    succeed note
        |. symbol "b"
