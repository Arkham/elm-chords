module Instruments.Guitar exposing
    ( Config
    , defaultTuning
    , voicings
    )

{-| A guitar representation.

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


{-| Configure your guitar by passing a tuning and the number of frets.
-}
type alias Config =
    { tuning : List Pitch
    , numFrets : Int
    }


{-| The default tuning of a guitar.
-}
defaultTuning : List Pitch
defaultTuning =
    [ Pitch.build E 2
    , Pitch.build A 2
    , Pitch.build D 3
    , Pitch.build G 3
    , Pitch.build B 3
    , Pitch.build E 4
    ]


{-| Returns a list of possible voicings of a chord on a guitar.
-}
voicings : Config -> Chord -> List Voicing
voicings config chord =
    let
        ( root, _ ) =
            Chords.toIntegerNotation chord
    in
    chord
        |> Fretted.voicings config
        |> List.filter (isErgonomic root)
        |> List.sortBy Fretted.sortIndex
        |> List.map (muteNonRoot root)


isErgonomic : Note -> Voicing -> Bool
isErgonomic root voicing =
    let
        { fretsCount, fretsAtMinimum, fretsAtMaximum } =
            Fretted.voicingInfo voicing

        hasNoHoles =
            fretsCount == List.length voicing

        hasLowerRoot =
            voicing
                |> List.take 3
                |> List.filterMap identity
                |> List.any
                    (\( _, note ) ->
                        Fretted.matchingNote note root
                    )
    in
    hasNoHoles
        && hasLowerRoot
        && (fretsAtMinimum > 1)
        && (fretsAtMaximum <= 3)


muteNonRoot : Note -> Voicing -> Voicing
muteNonRoot root voicing =
    case voicing of
        (Just ( _, first )) :: (Just ( _, second )) :: rest ->
            if Fretted.matchingNote first root || Fretted.matchingNote second root then
                voicing

            else
                Nothing :: Nothing :: rest

        other ->
            other
