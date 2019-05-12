# Elm Chords [![Build Status](https://travis-ci.com/Arkham/elm-chords.svg?branch=master)](https://travis-ci.com/Arkham/elm-chords)

Parse chords sheets for guitar and ukulele in Elm!

## Installation

`elm install Arkham/elm-chords`

## Example

Here's an example of what you could do:

```elm
import Html exposing (Html)
import Chords exposing (Chord, Voicing)
import Chords.Chart
import Instruments.Guitar as Guitar

chords : List String
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


view : Html msg
view =
    Html.div []
        (chords
            |> List.map
                (\name ->
                    ( name, Chords.parseChord name )
                )
            |> List.map
                (\( name, result ) ->
                    case result of
                        Ok chord ->
                            viewChord chord

                        Err err ->
                            Html.span []
                                [ Html.text ("Could not parse " ++ name)
                                ]
                )
        )


viewChord : Chord -> Html msg
viewChord chord =
    let
        config =
            { tuning = Guitar.defaultTuning
            , numFrets = 10
            }

        name =
            Chords.toString chord
    in
    case Guitar.voicings config chord of
        [] ->
            Html.span []
                [ Html.text
                    ("Could not find voicing for chord " ++ name)
                ]

        first :: rest ->
            Chords.Chart.view name first
```

This will parse the chords, generate some voicings and display the charts. You
should see something like this!

<img src="https://github.com/Arkham/elm-chords/blob/master/images/charts.png" width="800">

If you'd like to use Ukulele instead, import `Instruments.Ukulele` and use
it in the same way!

## Tests

Pull the repo and run `elm-test`
