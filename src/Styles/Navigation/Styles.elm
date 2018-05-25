module Navigation.Styles exposing (Styles(..), styles)

import Color exposing (rgba, white)
import Color.Manipulate
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (colors, colorDerivatives, scaled)
import Variations exposing (Variations(Active))


-- ⚗️


type Styles
    = Inside
    | InsideItem
    | Outside
    | OutsideItem



-- 🍯


styles : List (Style Styles Variations)
styles =
    List.concat [ outside, inside ]



-- Outside


outside : List (Style Styles Variations)
outside =
    [ -----------------------------------
      -- Container
      -----------------------------------
      style Outside
        [ Font.letterSpacing 1
        , Font.lineHeight 1.6
        , Font.size 11.5
        , Font.uppercase

        -- Font smoothing
        , Style.prop "-webkit-font-smoothing" "subpixel-antialiased"
        , Style.prop "-moz-osx-font-smoothing" "auto"
        , Style.prop "font-smooth" "never"
        ]

    -----------------------------------
    -- Item
    -----------------------------------
    , style OutsideItem
        [ opacity 0.55

        -- Active version
        , variation Active
            [ Border.bottom 1
            , Color.border (Color.Manipulate.fadeOut 0.875 colorDerivatives.text)
            , Color.text (Color.Manipulate.fadeOut 0.275 colorDerivatives.text)
            , opacity 1
            ]
        ]
    ]



-- Inside


inside : List (Style Styles Variations)
inside =
    [ -----------------------------------
      -- Container
      -----------------------------------
      style Inside
        [ Border.bottom 1
        , Color.background white
        , Color.border colorDerivatives.subtleBorder
        ]

    -----------------------------------
    -- Item
    -----------------------------------
    , style InsideItem
        [ Border.right 1
        , Color.border colorDerivatives.subtleBorder
        , Color.text colors.base02
        , Font.letterSpacing -0.165
        , Font.size (scaled -2)
        , Font.weight 600

        --
        , cursor "pointer"
        , pseudo "last-child" [ Border.right 0 ]
        ]
    ]
