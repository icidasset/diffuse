module Console.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (span, svg)
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
        , marginTop (gr 2)
        , textAlign center
        ]

    ------------------------------------------------------
    -- Buttons / Button
    ------------------------------------------------------
    , class ConsoleButton
        [ cursor pointer
        , height (gr 11)
        , lineHeight (gr 11)
        , padding2 zero (gr 6)
        , position relative

        --
        , children
            [ span
                [ fontSize (Css.rem (12 / 16))
                , fontWeight (int 700)
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
        , fontSize (Css.rem (13 / 16))
        , fontStyle italic
        , fontWeight (int 300)
        , padding2 (gr 2) zero
        , textAlign center
        ]

    ------------------------------------------------------
    -- Progress Bar
    ------------------------------------------------------
    , class ProgressBar
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
        ]
    ]
