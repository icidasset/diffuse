module Console.StylesOld exposing (..)

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
    [ ------------------------------------------------------
      -- Buttons / Button
      ------------------------------------------------------
      class ConsoleButton
        [ cursor pointer
        , height (gr 11)
        , lineHeight (gr 11)
        , padding2 zero (gr 2)
        , position relative
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
    ]
