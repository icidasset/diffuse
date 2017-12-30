module Tracks.OldStyles exposing (..)

import Color
import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (..)
import Variables exposing (colors, colorDerivatives)


type Classes
    = FavouritesOnly
    | NoTracksFound
    | NoTracksFoundUnderline
    | TracksContainer
    | TracksChild
    | TracksNavigation
    | TracksNavigationIcon
    | TracksNavigationIcons
    | TracksTable



-- 🦄


styles : List Snippet
styles =
    [ ------------------------------------------------------
      -- Table
      ------------------------------------------------------
      class TracksTable
        [ descendants
            [ --
              -- <thead>
              --
              th
                [ backgroundColor (hex "#fff")
                , borderBottom3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , borderLeft3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , color (hex "#CFCFCF")
                , cursor pointer
                , fontSize (Css.rem 0.65)
                , paddingLeft (gr 2)
                , paddingTop (px 3)
                , position relative
                , textAlign left
                ]
            , (th << toa << firstChild)
                [ borderLeft zero
                , width (gr 4)
                ]
            , (th << toa << descendants << toa << svg)
                [ fontSize (Css.rem 1)
                , position absolute
                , right (gr 1)
                , top (pct 50)
                , transform (translateY (pct -50))
                ]
            ]
        ]
    ]
