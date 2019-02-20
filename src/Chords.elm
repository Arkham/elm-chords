module Chords exposing
    ( Chord(..), Quality(..), TertianQuality(..)
    , toString, toIntegerNotation
    , Token(..), chordParser, lineParser
    , parseChord, parseLine, parseSheet
    , Fret, Voicing, voicingToString
    )

{-| Parse chords and chord sheets in Elm

@docs Chord, Quality, TertianQuality


# Converting

@docs toString, toIntegerNotation


# Parsing Chords

@docs Token, chordParser, lineParser
@docs parseChord, parseLine, parseSheet


# Chord Voicings

@docs Fret, Voicing, voicingToString

-}

import Chords.Note as Note exposing (Note(..))
import Chords.Pitch exposing (Pitch)
import Parser as P
    exposing
        ( (|.)
        , (|=)
        , Parser
        , oneOf
        , succeed
        , symbol
        )
import Set


{-| A chord has a note which gives it a name and a general quality.
-}
type Chord
    = Chord Note Quality


{-| The quality of the chord. Most well known chord types are described by
a tertian quality, which means a chord characterized by a root note, a
third and a fifth. We might want to have less notes, or more notes, or
replace the root note with another.
-}
type Quality
    = Fifth
    | Tertian TertianQuality
    | Sus2 TertianQuality
    | Sus4 TertianQuality
    | Add9 TertianQuality
    | Add11 TertianQuality
    | NewRoot Note TertianQuality


{-| This type contains chords characterized by a root note, a third (minor or
major) and a fifth (diminished, perfect or augmented)
-}
type TertianQuality
    = Major -- Root, Major Third, Perfect Fifth
    | Minor -- Root, Minor Third, Perfect Fifth
    | Augmented -- Root, Major Third, Augmented Fifth
    | Diminished -- Root, Minor Third, Diminished Fifth
    | Dominant7 -- Root, Major Third, Perfect Fifth, Minor Seventh
    | Major7 -- Root, Major Third, Perfect Fifth, Major Seventh
    | Minor7 -- Root, Minor Third, Perfect Fifth, Major Seventh
    | AugmentedDominant7 -- Root, Major Third, Augmented Fifth, Minor Seventh
    | AugmentedMajor7 -- Root, Major Third, Augmented Fifth, Major Seventh
    | Diminished7 -- Root, Minor Third, Diminished Fifth, Diminished Seventh
    | Major6 -- Root, Major Third, Diminished Fifth, Major Sixth
    | Minor6 -- Root, Minor Third, Diminished Fifth, Major Sixth
    | Dominant9 -- Root, Major Third, Perfect Fifth, Minor Seventh, Major Ninth
    | Major9 -- Root, Major Third, Perfect Fifth, Major Seventh, Major Ninth
    | Minor9 -- Root, Minor Third, Perfect Fifth, Minor Seventh, Major Ninth


{-| Converts a Chord to a string.
-}
toString : Chord -> String
toString (Chord root quality) =
    case quality of
        Fifth ->
            Note.toString root ++ "5"

        Tertian tertian ->
            Note.toString root
                ++ tertianToString tertian

        Sus2 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "sus2"

        Sus4 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "sus4"

        Add9 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "add9"

        Add11 tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "add11"

        NewRoot newRoot tertian ->
            Note.toString root
                ++ tertianToString tertian
                ++ "/"
                ++ Note.toString newRoot


tertianToString : TertianQuality -> String
tertianToString tertian =
    case tertian of
        Major ->
            ""

        Minor ->
            "m"

        Augmented ->
            "+"

        Diminished ->
            "dim"

        Dominant7 ->
            "7"

        Major7 ->
            "maj7"

        Minor7 ->
            "m7"

        AugmentedDominant7 ->
            "+7"

        AugmentedMajor7 ->
            "+M7"

        Diminished7 ->
            "dim7"

        Major6 ->
            "6"

        Minor6 ->
            "m6"

        Dominant9 ->
            "9"

        Major9 ->
            "maj9"

        Minor9 ->
            "m9"


{-| Converts a Chord to its integer notation, which is the distance in
semitones of every note from the root.
-}
toIntegerNotation : Chord -> ( Note, List Int )
toIntegerNotation (Chord root quality) =
    let
        sortAndUnique list =
            list
                |> Set.fromList
                |> Set.toList
                |> List.sort
    in
    case quality of
        Fifth ->
            ( root, [ 0, 5 ] )

        Tertian tertian ->
            ( root, sortAndUnique <| tertianToIntegerNotation tertian )

        Sus2 tertian ->
            ( root, sortAndUnique <| 2 :: tertianToIntegerNotation tertian )

        Sus4 tertian ->
            ( root, sortAndUnique <| 5 :: tertianToIntegerNotation tertian )

        Add9 tertian ->
            ( root, sortAndUnique <| 14 :: tertianToIntegerNotation tertian )

        Add11 tertian ->
            ( root, sortAndUnique <| 17 :: tertianToIntegerNotation tertian )

        NewRoot newRoot tertian ->
            let
                tertianNotation =
                    tertianToIntegerNotation tertian

                distanceBetweenRoots =
                    Note.distance newRoot root

                shiftedNotation =
                    List.map
                        (\note ->
                            modBy 12 (note + distanceBetweenRoots)
                        )
                        tertianNotation
            in
            ( newRoot, sortAndUnique <| 0 :: shiftedNotation )


tertianToIntegerNotation : TertianQuality -> List Int
tertianToIntegerNotation tertian =
    case tertian of
        Major ->
            [ 0, 4, 7 ]

        Minor ->
            [ 0, 3, 7 ]

        Augmented ->
            [ 0, 4, 8 ]

        Diminished ->
            [ 0, 3, 6 ]

        Dominant7 ->
            [ 0, 4, 7, 10 ]

        Major7 ->
            [ 0, 4, 7, 11 ]

        Minor7 ->
            [ 0, 3, 7, 10 ]

        AugmentedDominant7 ->
            [ 0, 4, 8, 10 ]

        AugmentedMajor7 ->
            [ 0, 4, 8, 11 ]

        Diminished7 ->
            [ 0, 3, 6, 9 ]

        Major6 ->
            [ 0, 4, 7, 9 ]

        Minor6 ->
            [ 0, 3, 7, 9 ]

        Dominant9 ->
            [ 0, 4, 7, 10, 14 ]

        Major9 ->
            [ 0, 4, 7, 11, 14 ]

        Minor9 ->
            [ 0, 3, 7, 10, 14 ]



-- CHORD PARSING


{-| Parses a string to a Chord. The return value is a result which either
returns a Chord or a String that describes why we couldn't parse successfully.
-}
parseChord : String -> Result (List P.DeadEnd) Chord
parseChord string =
    let
        -- we want to match the whole string
        endParser : Parser Chord
        endParser =
            succeed identity
                |= chordParser
                |. P.end
    in
    P.run endParser string


{-| The actual chord parser. Using this you can create your own line parser,
for example if you want to parse chords separated by spaces, or curly brackets.
-}
chordParser : Parser Chord
chordParser =
    let
        buildChord note tertian quality =
            Chord note (quality tertian)
    in
    succeed buildChord
        |= noteParser
        |= oneOf
            [ succeed identity
                |. P.backtrackable aug
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
            , succeed identity
                |. P.backtrackable maj
                |= oneOf
                    [ succeed Major9
                        |. symbol "9"
                    , succeed Major7
                        |. symbol "7"
                    ]
            , succeed Dominant9
                |. symbol "9"
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
                    [ succeed Minor9
                        |. symbol "9"
                    , succeed Minor7
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



-- LINE PARSING


{-| This type represents either a successfully parsed Chord or a portion of
the song lyrics. Thanks to the design of the line parser, the lyrics will
always be non-empty.
-}
type Token
    = Parsed Chord
    | Lyrics String


{-| Parses a line of a song to a list of Tokens. When we cannot parse the line
successfully, we will return a description of why the parsing failed. We will
also automatically merge adjacent lyrics tokens.
-}
parseLine : String -> Result (List P.DeadEnd) (List Token)
parseLine line =
    P.run lineParser line
        |> Result.map canonicalizeTokens


{-| Parses a full song line by line.
-}
parseSheet : String -> List (Result (List P.DeadEnd) (List Token))
parseSheet sheet =
    sheet
        |> String.lines
        |> List.map parseLine


canonicalizeTokens : List Token -> List Token
canonicalizeTokens tokens =
    let
        doIt list acc =
            case list of
                (Lyrics a) :: (Lyrics b) :: rest ->
                    doIt rest (Lyrics (a ++ b) :: acc)

                a :: rest ->
                    doIt rest (a :: acc)

                [] ->
                    List.reverse acc
    in
    doIt tokens []


{-| The actual line parser. You can use this if you want to customize the
line parsing, for example if you want to parse the whole song together.
-}
lineParser : Parser (List Token)
lineParser =
    succeed identity
        |= P.loop [] lineParserHelper


lineParserHelper : List Token -> Parser (P.Step (List Token) (List Token))
lineParserHelper acc =
    oneOf
        [ succeed identity
            |. symbol "["
            |= oneOf
                [ succeed (\v -> P.Loop (Parsed v :: acc))
                    |= chordParser
                    |. symbol "]"
                , succeed (P.Loop acc)
                    |. symbol "]"
                ]
        , succeed (\v -> P.Loop (Lyrics v :: acc))
            |= lyricsParser
        , succeed (P.Done (List.reverse acc))
        ]


lyricsParser : Parser String
lyricsParser =
    P.getChompedString
        (succeed ()
            -- we want to match at least one character
            |. P.chompIf (\_ -> True)
            -- and keep doing it until we meet [
            |. P.chompWhile (\c -> c /= '[')
        )



-- VOICINGS


{-| Which fret it is being pressed.
-}
type alias Fret =
    Int


{-| For each string of our instrument, we could be muting the string, playing
an open string or pressing a particular fret.
-}
type alias Voicing =
    List (Maybe ( Fret, Pitch ))


{-| Converts a Voicing to a compact string representation. For example, the
C Major chord is written as "032010", while the D Major chord is written as
"XX0232".
-}
voicingToString : Voicing -> String
voicingToString voicing =
    voicing
        |> List.map
            (\elem ->
                case elem of
                    Just ( fret, note ) ->
                        String.fromInt fret

                    Nothing ->
                        "X"
            )
        |> String.join ""
