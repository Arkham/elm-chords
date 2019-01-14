module Chords exposing (parseSheet)


type Chord
    = Chord String


type alias PositionedChord =
    ( Chord, Int )


parseSheet : String -> List ( String, List PositionedChord )
parseSheet sheet =
    []
