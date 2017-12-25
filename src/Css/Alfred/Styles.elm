module Alfred.Styles exposing (Styles(..), styles)

import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (colorDerivatives, scaled)
import Variations exposing (Variations(Active))


-- ⚗️


type Styles
    = Container
    | Input
    | Message
    | Results
    | ResultItem



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Container
      -----------------------------------
      style Container
        [ Color.text Color.white ]
    , -----------------------------------
      -- Input
      -----------------------------------
      style Input
        [ Border.rounded 3
        , Color.background Color.white
        , Color.placeholder (Color.greyscale 0.25)
        , Color.text colorDerivatives.text
        , Font.size (scaled 4)

        --
        , prop
            "box-shadow"
            "0 1px 3px 0 rgba(0, 0, 0, 0.225), 0 3px 15px 0 rgba(0, 0, 0, 0.1) !important"
        ]
    , -----------------------------------
      -- Message
      -----------------------------------
      style Message
        [ Font.italic
        , Font.size (scaled 0)
        ]
    , -----------------------------------
      -- Results
      -----------------------------------
      style Results
        [ Border.rounded 3
        , Color.background (Color.rgba 0 0 0 0.75)
        , Color.text Color.white
        , Font.size (scaled -1)
        ]
    , -----------------------------------
      -- Result item
      -----------------------------------
      let
        activeItemStyles =
            [ Font.bold
            , scaled 2
                |> toString
                |> (\x -> x ++ "px")
                |> prop "padding-left"
            ]
      in
        style ResultItem
            [ Border.bottom 1
            , Color.border (Color.rgba 255 255 255 0.15)

            --
            , cursor "pointer"
            , pseudo "hover" activeItemStyles
            , variation Active activeItemStyles
            ]
    ]
