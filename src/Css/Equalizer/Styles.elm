module Equalizer.Styles exposing (..)

import Color
import Css exposing (..)
import Css.Elements exposing (div)
import Traits exposing (..)
import Variables exposing (colors, colorDerivatives)


type Classes
    = Equalizer
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
    [ class Equalizer
        [ border3 (px 1) solid (cssColorOpac 0.05 colors.base00)
        , displayFlex
        , left (pct 50)
        , marginTop (gr 3)
        , position absolute
        , top (pct 50)
        , transform (translate2 (pct -50) (pct -50))
        ]

    --
    -- Knob Column
    --
    , class KnobColumn
        [ alignItems center
        , borderRight3 (px 1) solid (cssColorOpac 0.05 colors.base00)
        , displayFlex
        , flexDirection column
        , padding3 (gr 3) (gr 8) (gr 2)

        --
        , lastChild
            [ borderRight zero ]
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
        , (children << List.singleton << div)
            [ borderLeft3 (px 1) solid (cssColorOpac knobOpac knobColor)
            , borderRight3 (px 1) solid (cssColorOpac knobOpac knobColor)
            , height (px 8)
            , left zero
            , marginTop (px -4)
            , position absolute
            , right zero
            , top (pct 50)

            --
            , nthChild "2" [ transform <| rotate (deg (36 * 1)) ]
            , nthChild "3" [ transform <| rotate (deg (36 * 2)) ]
            , nthChild "4" [ transform <| rotate (deg (36 * 3)) ]
            , nthChild "5" [ transform <| rotate (deg (36 * 4)) ]
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
            mixin
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
