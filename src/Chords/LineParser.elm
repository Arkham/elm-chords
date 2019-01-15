module Chords.LineParser exposing (parseLine)

import Chords.Types exposing (Chord(..), Token(..))
import Parser exposing (..)


parseLine : String -> List Token
parseLine line =
    case run lineParser line of
        Ok tokens ->
            canonicalize tokens

        Err err ->
            [ Text line ]


canonicalize : List Token -> List Token
canonicalize tokens =
    let
        doIt list acc =
            case list of
                (Text a) :: (Text b) :: rest ->
                    doIt rest (Text (a ++ b) :: acc)

                a :: rest ->
                    doIt rest (a :: acc)

                [] ->
                    List.reverse acc
    in
    doIt tokens []


lineParser : Parser (List Token)
lineParser =
    succeed identity
        |= loop [] lineParserHelp


lineParserHelp :
    List Token
    -> Parser (Step (List Token) (List Token))
lineParserHelp revTokens =
    oneOf
        [ succeed identity
            |. symbol "["
            |= oneOf
                [ succeed (\c -> Loop (Parsed c :: revTokens))
                    |= chordParser
                    |. symbol "]"
                , succeed (Loop revTokens)
                    |. symbol "]"
                ]
        , succeed (\t -> Loop (Text t :: revTokens))
            |= textParser
        , succeed ()
            |> map (\_ -> Done (List.reverse revTokens))
        ]


chordParser : Parser Chord
chordParser =
    succeed Chord
        |= getChompedString
            (succeed ()
                |. chompIf (not << isClosing)
                |. chompWhile (not << isClosing)
            )


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
