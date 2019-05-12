module Instruments.Ukulele exposing
    ( Config
    , defaultTuning
    , voicings
    )

{-| A ukulele representation.

@docs Config


# Defaults

@docs defaultTuning


# Find voicings

@docs voicings

-}

import Chords exposing (Chord(..), Voicing)
import Chords.Note as Note exposing (Note(..))
import Chords.Pitch as Pitch exposing (Pitch)
import Instruments.Fretted as Fretted


{-| Configure your ukulele by passing a tuning and the number of frets.
-}
type alias Config =
    { tuning : List Pitch
    , numFrets : Int
    }


{-| The default tuning of a ukulele.
-}
defaultTuning : List Pitch
defaultTuning =
    [ Pitch.build G 3
    , Pitch.build C 3
    , Pitch.build E 3
    , Pitch.build A 3
    ]


{-| Returns a list of possible voicings of a chord on a ukulele.
-}
voicings : Config -> Chord -> List Voicing
voicings config chord =
    Fretted.voicings config chord
        |> List.filter isErgonomic
        |> List.sortBy Fretted.sortIndex


isErgonomic : Voicing -> Bool
isErgonomic voicing =
    let
        { fretsCount } =
            Fretted.voicingInfo voicing

        hasNoHoles =
            fretsCount == List.length voicing
    in
    hasNoHoles
