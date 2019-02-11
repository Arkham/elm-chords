# Elm Chords [![Build Status](https://travis-ci.com/Arkham/elm-chords.svg?branch=master)](https://travis-ci.com/Arkham/elm-chords)

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

This will parse the chords and generate some voicings, which you will be able to
export to snazzy SVGs using `Chords.Instruments.Chart.view` :)

## Tests

Pull the repo and run `elm-test`
