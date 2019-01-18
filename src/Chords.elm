module Chords exposing (parseTab)

import Chords.LineParser as LineParser exposing (Token)


parseTab : String -> List (List Token)
parseTab sheet =
    sheet
        |> String.split "\n"
        |> List.map LineParser.parse
