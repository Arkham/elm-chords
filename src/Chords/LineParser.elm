module Chords.LineParser exposing (parse)

import Chords.ChordParser as ChordParser
import Chords.Types exposing (Chord(..), Token(..))
import Parser exposing (..)


parse : String -> List Token
parse line =
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
                    |= ChordParser.parser
                    |. symbol "]"
                , succeed (Loop revTokens)
                    |. symbol "]"
                ]
        , succeed (\t -> Loop (Text t :: revTokens))
            |= textParser
        , succeed ()
            |> map (\_ -> Done (List.reverse revTokens))
        ]


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
