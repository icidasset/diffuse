module StylesOld exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Media exposing (withMedia)
import Traits exposing (..)
import Variables exposing (..)


-- Children

import Tracks.Styles as Tracks


styles : List Snippet
styles =
    stylesLocal
        |> List.append Tracks.styles


keyframes : String
keyframes =
    String.concat
        []



-- 🦄


type Classes
    = AuthenticationButton
    | BackgroundImage
    | Basic
    | Button
    | ButtonSmall
    | ButtonSubtle
    | Columns
    | ContentBox
    | EmptyState
    | Important
    | Insulation
    | InsulationCentered
    | InsulationContent
    | InsulationFlexContent
    | InTheMiddle
    | Intro
    | LogoBackdrop
    | Overlay
    | OverlayWithCursor
    | Shell


stylesLocal : List Snippet
stylesLocal =
    [ ------------------------------------------------------
      -- <🎃>
      ------------------------------------------------------
      h3
        [ fontSize (Css.rem 1.4)
        , Traits.headerFont
        , margin3 (gr -2) zero (gr 6)
        , textAlign left
        ]

    --                          --
    --                          --
    --  OTHER BASIC COMPONENTS  --
    --                          --
    --                          --
    ------------------------------------------------------
    -- Columns
    ------------------------------------------------------
    , class Columns
        [ property "columns" "2"
        , property "column-gap" (.value (gr 8))

        --
        , children
            [ div
                [ property "-webkit-column-break-inside" "avoid"
                , property "break-inside" "avoid"
                , property "page-break-inside" "avoid"
                ]
            , p
                [ margin zero
                ]
            ]
        ]

    ------------------------------------------------------
    -- Empty state
    ------------------------------------------------------
    , class EmptyState
        [ color (cssColor colors.base06)
        , fontWeight (int 600)
        , left (pct 50)
        , lineHeight (num 1.45)
        , marginTop (gr 3)
        , position absolute
        , textAlign center
        , top (pct 50)
        , transform (translate2 (pct -50) (pct -50))

        --
        , descendants
            [ svg
                [ fontSize (basem 64)
                , marginBottom (gr 1)
                ]
            , selector "g"
                [ fill currentColor
                ]
            ]
        ]

    ------------------------------------------------------
    -- Important text
    ------------------------------------------------------
    , class Important
        [ alignItems center
        , border3
            (px 2)
            solid
            (colors.base08
                |> cssColor
            )
        , borderRadius (px 3)
        , color (cssColor colors.base08)
        , displayFlex
        , fontWeight (int 700)
        , lineHeight (int 1)
        , padding2 (gr 2) (gr 2)

        --
        , descendants
            [ selector "svg g"
                [ fill currentColor ]
            ]
        ]
    ]
