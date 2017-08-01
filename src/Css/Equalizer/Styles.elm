module Equalizer.Styles exposing (..)

import Color
import Css exposing (..)
import Css.Elements exposing (div, h1, svg)
import Traits exposing (..)
import Variables exposing (colors, colorDerivatives)


type Classes
    = Equalizer
    | EqualizerContainer
    | InsulationForEqualizer
    | KnobColumn
    | KnobLabel
    | KnobLayerA
    | KnobLayerB
    | KnobLayerC
    | KnobLines
    | Knob



-- Variables


knobColor : Color.Color
knobColor =
    colors.base03


knobSize : Float
knobSize =
    36


knobOpac : Float
knobOpac =
    0.7



-- 🦄


styles : List Snippet
styles =
    [ class EqualizerContainer
        [ alignItems center
        , displayFlex
        , flex (int 1)
        , justifyContent center
        , position relative

        --
        , descendants
            [ h1
                [ left (gr 4)
                , position absolute
                , top zero
                ]
            ]
        ]
    , class InsulationForEqualizer
        [ displayFlex
        , flexDirection column
        ]

    --
    -- EQ
    --
    , class Equalizer
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        , marginTop (gr -1)
        ]

    --
    -- Knob Column
    --
    , class KnobColumn
        [ alignItems center
        , border3 (px 1) solid (hex "#E6EAEB")
        , displayFlex
        , flexDirection column
        , margin4 (px -1) zero zero (px -1)
        , padding3 (gr 3) (gr 8) (gr 2)
        ]

    --
    -- Knob Label
    --
    , class KnobLabel
        [ color (cssColorOpac 0.4 Color.black)
        , fontSize (Css.rem 0.525)
        , fontWeight (int 700)
        , letterSpacing (Css.em 0.05)
        , lineHeight (num 1.45)
        , marginTop (gr 2)
        , textTransform uppercase
        ]

    --
    -- Knob
    --
    , class Knob
        [ borderRadius (pct 50)
        , boxShadow6 inset (px 0) (px 0) (px 5) (px 1) (cssColorOpac (knobOpac - 0.35) knobColor)
        , cursor pointer
        , height (px knobSize)
        , position relative
        , width (px knobSize)
        ]

    --
    , each
        [ class KnobLayerA, class KnobLayerB, class KnobLayerC ]
        [ borderRadius (pct 50), position absolute ]

    --
    , class KnobLayerA
        [ bottom (px 3)
        , left (px 3)
        , right (px 3)
        , top (px 3)
        , zIndex (int 1)
        ]

    --
    , class KnobLayerB
        [ bottom (px 5)
        , left (px 5)
        , right (px 5)
        , top (px 5)
        , zIndex (int 2)

        --
        , descendants
            [ svg
                [ fill transparent
                , height (px (knobSize - 9))
                , left (pct 50)
                , position absolute
                , property "stroke" (cssColorOpac knobOpac knobColor |> .value)
                , property "stroke-linejoin" "miter"
                , property "stroke-width" "7px"
                , top (pct 50)
                , transforms [ rotate (deg 90), translate2 (pct -50) (pct 50) ]
                , width (px (knobSize - 9))
                ]
            ]
        ]

    --
    , class KnobLayerC
        [ bottom (px 8)
        , left (px 8)
        , right (px 8)
        , top (px 8)
        , zIndex (int 3)

        --
        , boxShadow5 (px 0) (px 0) (px 3) (px 1) (cssColorOpac (knobOpac + 0.3) knobColor)

        --
        , after
            [ backgroundColor (cssColorOpac (knobOpac + 0.1) knobColor)
            , height (px 9)
            , left (pct 50)
            , position absolute
            , property "content" "''"
            , top zero
            , transform (translateX (pct -50))
            , width (px 2)
            , zIndex (int 4)
            ]
        ]

    --
    , let
        pseudo =
            batch
                [ backgroundColor (cssColorOpac (knobOpac + 0.1) knobColor)
                , bottom zero
                , height (px 9)
                , position absolute
                , property "content" "''"
                , width (px 1)
                ]
      in
        class KnobLines
            [ bottom (px -2)
            , left (px 1)
            , position absolute
            , right (px 1)
            , zIndex (int 0)

            --
            , before
                [ pseudo
                , left zero
                , transform (rotate <| deg 45)
                ]

            --
            , after
                [ pseudo
                , right zero
                , transform (rotate <| deg -45)
                ]
            ]
    ]
