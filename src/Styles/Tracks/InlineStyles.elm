module Tracks.InlineStyles exposing (..)

import Color.Convert
import Tracks.Styles exposing (trackHeight)
import Tracks.Types
import Variables exposing (colors, scaledStr)


-- ⚗️


type alias Styles =
    List ( String, String )



-- 🏓  /  Table


table : Styles
table =
    [ ( "display", "table" )
    , ( "list-style", "none" )
    , ( "margin", "5px 0" )
    , ( "padding", "0" )
    , ( "table-layout", "fixed" )
    , ( "white-space", "nowrap" )
    , ( "width", "100%" )
    ]



-- 🏓  /  Table  /  Rows


tableRow : Styles
tableRow =
    [ ( "background-color", "rgb(255, 255, 255)" ) ]


tableRowAlt : Styles
tableRowAlt =
    [ ( "background-color", "rgb(252, 252, 252)" ) ]


tableRowIsMissing : Styles
tableRowIsMissing =
    [ ( "color", "#8f8f8f" ) ]


tableRowIsNotMissing : Styles
tableRowIsNotMissing =
    [ ( "cursor", "pointer" ) ]


tableRowNowPlaying : Styles
tableRowNowPlaying =
    [ ( "background-color", Color.Convert.colorToCssRgb colors.base0D )
    , ( "color", "rgb(255, 255, 255)" )
    ]



-- 🏓  /  Table  /  Columns  /  Favourite


tableFavouriteColumn : Tracks.Types.Identifiers -> Styles
tableFavouriteColumn opts =
    [ ( "display", "table-cell" )
    , ( "font-family", "or-favourites" )
    , ( "height", toString trackHeight ++ "px" )
    , ( "padding-left", scaledStr -2 )
    , ( "padding-right", scaledStr -8 )

    --
    , ( "color"
      , if opts.isNowPlaying then
            "rgba(255, 255, 255, 0.4)"
        else if opts.isFavourite then
            Color.Convert.colorToCssRgb colors.base08
        else
            "rgb(222, 222, 222)"
      )
    ]



-- 🏓  /  Table  /  Columns  /  Other


tableOtherColumns : Styles
tableOtherColumns =
    [ ( "display", "table-cell" )
    , ( "height", toString trackHeight ++ "px" )
    , ( "overflow", "hidden" )
    , ( "padding-left", scaledStr -8 )
    , ( "padding-right", scaledStr -8 )
    , ( "text-overflow", "ellipsis" )
    ]
