# Elm Chords

Parse guitar chords and chord sheets in Elm!

## Installation

`elm install Arkham/elm-chords`

## Example

Here's an example of what you could do:

```elm
import Chords.ChordParser as CP
import Chords.Instruments.Guitar as Guitar
import Chords.Instruments.Voicing exposing (Voicing)

chords =
    [ "Am"
    , "E"
    , "C"
    , "Dm7"
    , "G"
    , "F"
    , "A#"
    , "C#"
    ]

getVoicings : List String -> List Voicing
getVoicings chords =
    let
        guitar =
            { tuning = Guitar.defaultTuning
            , numFrets = 10
            }
    in
    chords
        |> List.map CP.parse
        |> List.map
            (\result ->
                case result of
                    Ok chord ->
                        Guitar.voicings guitar chord
                        |> List.head

                    Err err ->
                        Nothing
            )
        |> List.filterMap identity
```

This will parse the chords and generate some voicings for those chords, which
you will be able to display to SVG using `Chords.Instruments.Diagram.view` :)
