module Console.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (label, span, svg)
import Traits exposing (cssColor, gr)
import Variables exposing (..)


type Classes
    = Console
    | ConsoleButton
    | ConsoleButtonLight
    | ConsoleButtonLightOn
    | ConsoleButtonLightExtended
    | ConsoleButtonLightExtendedOn
    | ConsoleButtonsContainer
    | NowPlaying
    | ProgressBar
    | ProgressBarInner
    | ProgressBarValue



-- 🦄


styles : List Snippet
styles =
    [ class Console
        [ margin3 (px 1) auto zero
        , maxWidth insulationWidth
        , width (pct 100)
        ]

    ------------------------------------------------------
    -- Buttons / Container
    ------------------------------------------------------
    , class ConsoleButtonsContainer
        [ color (cssColor colorDerivatives.consoleText)
        , displayFlex
        , justifyContent center
        , marginTop (gr 1)
        , textAlign center
        ]

    ------------------------------------------------------
    -- Buttons / Button
    ------------------------------------------------------
    , class ConsoleButton
        [ cursor pointer
        , height (gr 11)
        , lineHeight (gr 11)
        , margin2 zero (gr 4)
        , padding2 zero (gr 2)
        , position relative

        --
        , children
            [ label
                [ cursor inherit
                , fontSize (Traits.basem 12)
                , fontWeight (int 900)
                , letterSpacing (Css.em 0.25)
                ]
            , svg
                [ verticalAlign middle
                ]
            ]
        ]

    ------------------------------------------------------
    -- Buttons / Light
    ------------------------------------------------------
    , class ConsoleButtonLight
        [ backgroundColor (rgba 255 255 255 0.25)
        , borderRadius (px 2)
        , height (px 4)
        , left (pct 50)
        , position absolute
        , top (pct 50)
        , transform (translate2 (pct -50) (px -23))
        , width (px 4)
        ]

    -- Extended
    , class ConsoleButtonLightExtended [ width (px 17) ]

    -- On
    , class ConsoleButtonLightOn [ backgroundColor (hex "#9DAEFF") ]
    , class ConsoleButtonLightExtendedOn [ backgroundColor (hex "#C6FE99") ]

    ------------------------------------------------------
    -- Now Playing
    ------------------------------------------------------
    , class NowPlaying
        [ color (rgba 255 255 255 0.8)
        , fontSize (Traits.basem 13)
        , fontStyle italic
        , padding3 (gr 3) zero (gr 2)
        , textAlign center
        ]

    ------------------------------------------------------
    -- Progress Bar
    ------------------------------------------------------
    , class ProgressBar
        [ cursor pointer
        , height (px 3)
        , padding2 (gr 1) zero
        ]
    , class ProgressBarInner
        [ backgroundColor (rgba 255 255 255 0.25)
        , borderRadius (px 3)
        , height (px 3)
        , position relative
        ]
    , class ProgressBarValue
        [ backgroundColor (rgba 255 255 255 0.325)
        , height (pct 100)
        , left zero
        , position absolute
        , top zero

        --
        , property "transition" "width 250ms ease"
        ]
    ]
