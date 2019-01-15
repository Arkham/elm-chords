module Chords.LineParser exposing (parseLine)

import Chords.Types exposing (Chord(..), Token(..))
import Parser exposing (..)


parseLine : String -> List Token
parseLine line =
    case run lineParser line of
        Ok tokens ->
            tokens

        Err err ->
            [ Text line ]


lineParser : Parser (List Token)
lineParser =
    succeed identity
        |= loop [] lineParserHelp


lineParserHelp :
    List Token
    -> Parser (Step (List Token) (List Token))
lineParserHelp revTokens =
    oneOf
        [ succeed (\c -> Loop (Parsed c :: revTokens))
            |= chordParser
        , succeed (\t -> Loop (Text t :: revTokens))
            |= textParser
        , succeed ()
            |> map (\_ -> Done (List.reverse revTokens))
        ]


chordParser : Parser Chord
chordParser =
    succeed Chord
        |. symbol "["
        |= getChompedString (chompWhile (not << isClosing))
        |. symbol "]"


textParser : Parser String
textParser =
    getChompedString <|
        succeed ()
            |. chompIf (\_ -> True)
            |. chompWhile (not << isOpening)


isClosing : Char -> Bool
isClosing =
    (==) ']'


isOpening : Char -> Bool
isOpening =
    (==) '['
