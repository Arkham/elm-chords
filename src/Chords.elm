module Chords exposing (parseSheet)

import Chords.LineParser as LineParser
import Chords.Types exposing (Token)


parseSheet : String -> List (List Token)
parseSheet sheet =
    sheet
        |> String.split "\n"
        |> List.map LineParser.parseLine
