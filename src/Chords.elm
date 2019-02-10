module Chords exposing (parseSheet)

{-|


# Chords

This module lets you parse guitar chords and chord sheets.

@docs parseSheet

-}

import Chords.LineParser as LineParser exposing (Token)


{-| It parses a multiline chord sheet.
-}
parseSheet : String -> List (List Token)
parseSheet sheet =
    sheet
        |> String.split "\n"
        |> List.map LineParser.parse
