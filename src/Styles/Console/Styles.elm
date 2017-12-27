module Console.Styles exposing (Styles(..), styles)

import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (colorDerivatives, scaled)
import Variations exposing (Variations)


-- ⚗️


type Styles
    = Button
    | ButtonLabel
    | ButtonLight
    | Container
    | NowPlaying
    | ProgressBar
    | ProgressBarContainer
    | ProgressBarValue



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Buttons
      -----------------------------------
      style Button
        [ cursor "pointer" ]

    --
    , style ButtonLabel
        [ Font.letterSpacing 3
        , Font.size 12
        , Font.weight 900
        ]

    --
    , style ButtonLight
        [ Border.rounded 2
        , Color.background (Color.rgba 255 255 255 0.25)
        ]

    -----------------------------------
    -- Container
    -----------------------------------
    , style Container
        [ Color.text colorDerivatives.consoleText ]

    -----------------------------------
    -- Now playing
    -----------------------------------
    , style NowPlaying
        [ Font.italic
        , Font.size (scaled -1)
        ]

    -----------------------------------
    -- Progress bar
    -----------------------------------
    , style ProgressBar
        [ Border.rounded 3
        , Color.background (Color.rgba 255 255 255 0.25)
        ]

    --
    , style ProgressBarContainer [ cursor "pointer" ]
    , style ProgressBarValue [ Color.background (Color.rgba 255 255 255 0.325) ]
    ]
