module Chords.Instruments.Chart exposing (view, viewWith)

import Chords.Instruments.Voicing exposing (Voicing)
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes as Attr


type alias Config =
    { height : Int
    , width : Int
    , hPaddingPct : Float
    , vPaddingPct : Float
    , minFrets : Int
    }


view : Voicing -> Svg msg
view voicing =
    viewWith
        { height = 200
        , width = 150
        , hPaddingPct = 0.2
        , vPaddingPct = 0.2
        , minFrets = 4
        }
        voicing


viewWith : Config -> Voicing -> Svg msg
viewWith config voicing =
    let
        { height, width, hPaddingPct, vPaddingPct, minFrets } =
            config

        height_ =
            toFloat height

        width_ =
            toFloat width

        horizontalPadding =
            width_ * hPaddingPct

        verticalPadding =
            height_ * vPaddingPct

        numStrings =
            List.length voicing

        chartHeight =
            height_ - (2 * verticalPadding)

        chartWidth =
            width_ - (2 * horizontalPadding)

        columnWidth =
            chartWidth
                / toFloat (numStrings - 1)

        fretValues =
            List.foldl
                (\elem acc ->
                    case elem of
                        Just ( fret, note ) ->
                            fret :: acc

                        Nothing ->
                            acc
                )
                []
                voicing

        highestFret =
            List.maximum fretValues
                |> Maybe.withDefault 0

        lowestFret =
            List.minimum fretValues
                |> Maybe.withDefault 0

        fretRange =
            if lowestFret == 0 then
                highestFret - lowestFret

            else
                highestFret - lowestFret + 1

        numFrets =
            1 + max minFrets fretRange

        atBeginning =
            highestFret - numFrets < 0

        rowHeight =
            chartHeight
                / toFloat (numFrets - 1)

        strings =
            voicing
                |> List.indexedMap
                    (\index _ ->
                        let
                            horizontalOffset =
                                horizontalPadding
                                    + (columnWidth * toFloat index)

                            dVal =
                                d
                                    ( horizontalOffset, verticalPadding )
                                    ( horizontalOffset, verticalPadding + chartHeight )
                        in
                        Svg.path
                            [ Attr.stroke color
                            , Attr.d dVal
                            ]
                            []
                    )

        frets =
            List.range 0 (numFrets - 1)
                |> List.map
                    (\relativeFret ->
                        let
                            verticalOffset =
                                verticalPadding
                                    + (rowHeight * toFloat relativeFret)

                            strokeWidth =
                                if atBeginning && relativeFret == 0 then
                                    "4"

                                else
                                    "1"

                            dVal =
                                d
                                    ( horizontalPadding, verticalOffset )
                                    ( horizontalPadding + chartWidth, verticalOffset )
                        in
                        Svg.path
                            [ Attr.stroke color
                            , Attr.strokeWidth strokeWidth
                            , Attr.d dVal
                            ]
                            []
                    )

        notes =
            voicing
                |> List.indexedMap
                    (\index elem ->
                        case elem of
                            Just ( fret, note ) ->
                                let
                                    relativeFret =
                                        if lowestFret == 0 then
                                            fret - lowestFret

                                        else
                                            fret - lowestFret + 1

                                    verticalOffset =
                                        verticalPadding
                                            + (rowHeight * toFloat relativeFret)
                                            - (rowHeight / 2)

                                    horizontalOffset =
                                        horizontalPadding
                                            + (columnWidth * toFloat index)

                                    fillColor =
                                        if relativeFret == 0 && lowestFret == 0 then
                                            "white"

                                        else
                                            color
                                in
                                Svg.circle
                                    [ Attr.cx (String.fromFloat horizontalOffset)
                                    , Attr.cy (String.fromFloat verticalOffset)
                                    , Attr.r <| String.fromFloat (rowHeight / 5)
                                    , Attr.stroke color
                                    , Attr.fill fillColor
                                    ]
                                    []

                            Nothing ->
                                let
                                    verticalOffset =
                                        verticalPadding - (rowHeight / 2)

                                    horizontalOffset =
                                        horizontalPadding
                                            + (columnWidth * toFloat index)
                                in
                                Svg.text_
                                    [ Attr.x <| String.fromFloat horizontalOffset
                                    , Attr.y <| String.fromFloat verticalOffset
                                    , Attr.fontFamily "Arial"
                                    , Attr.fontSize <| String.fromFloat (rowHeight / 2)
                                    , Attr.textAnchor "middle"
                                    ]
                                    [ Svg.tspan
                                        [ Attr.alignmentBaseline "central"
                                        ]
                                        [ Svg.text "X"
                                        ]
                                    ]
                    )

        startFret =
            if atBeginning then
                []

            else
                [ Svg.text_
                    [ Attr.x <| String.fromFloat (horizontalPadding / 2)
                    , Attr.y <| String.fromFloat (rowHeight / 2 + verticalPadding)
                    , Attr.fontFamily "Arial"
                    , Attr.fontSize <| String.fromFloat (rowHeight / 2)
                    , Attr.textAnchor "middle"
                    ]
                    [ Svg.tspan
                        [ Attr.alignmentBaseline "central"
                        ]
                        [ Svg.text <| String.fromInt lowestFret
                        ]
                    ]
                ]
    in
    Svg.svg
        [ Attr.height (String.fromInt height)
        , Attr.width (String.fromInt width)
        ]
        (strings
            ++ frets
            ++ notes
            ++ startFret
        )


color : String
color =
    "black"


d : ( Float, Float ) -> ( Float, Float ) -> String
d ( startX, startY ) ( endX, endY ) =
    "M"
        ++ String.fromFloat startX
        ++ ","
        ++ String.fromFloat startY
        ++ " L"
        ++ String.fromFloat endX
        ++ ","
        ++ String.fromFloat endY
