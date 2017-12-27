module StylesOld exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Media exposing (withMedia)
import Traits exposing (..)
import Variables exposing (..)


-- Children

import Form.StylesOld as Form
import List.Styles as List
import Tracks.Styles as Tracks


styles : List Snippet
styles =
    stylesLocal
        |> List.append Form.styles
        |> List.append List.styles
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
      h2
        [ fontSize (Css.rem 1.4)
        , Traits.headerFont
        , margin3 (gr -2) zero (gr 4)
        , textAlign center
        ]
    , h3
        [ fontSize (Css.rem 1.4)
        , Traits.headerFont
        , margin3 (gr -2) zero (gr 6)
        , textAlign left
        ]
    , h4
        [ color (cssColor colors.base06)
        , fontSize (Css.rem 0.75)
        , fontWeight (int 500)
        , Traits.headerFont
        , textTransform uppercase

        --
        , descendants
            [ svg
                [ display inlineBlock
                , height (Css.em 1.1)
                , marginRight (px 8)
                , transform (translateY (px -1))
                , verticalAlign middle
                , width (Css.em 1.1)
                ]
            , selector "g"
                [ fill currentColor
                ]
            ]
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

    ------------------------------------------------------
    -- Logo backdrop
    ------------------------------------------------------
    , class LogoBackdrop
        [ backgroundImage (url "/images/icon-dark.svg")
        , backgroundPosition2 (px -124) (pct 53.75)
        , backgroundRepeat noRepeat
        , backgroundSize cover
        , bottom zero
        , left zero
        , opacity (num 0.025)
        , position absolute
        , right zero
        , top zero
        ]
    ]
