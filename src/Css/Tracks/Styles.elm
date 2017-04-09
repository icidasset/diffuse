module Tracks.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (svg, table, td, th, tr)
import Traits exposing (cssColor, gr)
import Variables exposing (colorDerivatives)


type Classes
    = TracksContainer
    | TracksTable
    | TracksTableContainer



-- 🦄


styles : List Snippet
styles =
    [ class TracksContainer
        [ displayFlex
        , flexDirection column
        ]

    ------------------------------------------------------
    -- Table
    ------------------------------------------------------
    , class TracksTableContainer
        [ flex (int 1)
        , overflow scroll
        ]
    , class TracksTable
        [ borderCollapse collapse
        , width (pct 100)

        --
        , descendants
            [ th
                [ borderBottom3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , borderLeft3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , color (hex "#CFCFCF")
                , fontSize (Traits.basem 12)
                , paddingLeft (gr 2)
                , textAlign left
                ]
            , (th << toa << firstChild)
                [ borderLeft zero
                , width (gr 4)
                ]
            , td
                [ cursor pointer
                , fontSize (Traits.basem 12)
                , lineHeight (num 1.6)
                , padding2 (gr 1) (gr 2)
                ]
            , (td << toa << descendants << toa << svg)
                [ display inlineBlock
                , verticalAlign textBottom
                ]

            -- Add some extra space on top
            , (tr << toa << firstChild << toa << descendants << toa << td)
                [ paddingTop (gr 2)
                ]
            ]
        ]
    ]



-- Utils


toa : a -> List a
toa =
    List.singleton
