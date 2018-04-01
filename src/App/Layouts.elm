module Layouts exposing (..)

import Color exposing (Color)
import Color.Convert
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Element.Types exposing (Attr, Node)
import Html
import Html.Attributes
import Html.Events
import Material.Icons.Navigation as Icons
import Svg
import Types as TopLevel
import Variables exposing (colorDerivatives, colors, scaled)


-- Styles

import Form.Styles exposing (Styles(..))
import List.Styles exposing (Styles(..))
import Styles exposing (Styles(..))


-- 🍯


btn : Styles.Styles -> List Attr -> Node -> Node
btn buttonStyles additionalAttributes buttonChild =
    el
        buttonStyles
        (List.append
            [ paddingXY (scaled -5) (scaled -10) ]
            (additionalAttributes)
        )
        buttonChild


emptyState : (Color -> Int -> Svg.Svg TopLevel.Msg) -> List Node -> Node
emptyState icon textNodes =
    [ 64
        |> icon colors.base06
        |> html
    , column
        Zed
        []
        textNodes
    ]
        |> column EmptyState [ center, spacingXY 0 (scaled 1) ]
        |> el Zed [ center, verticalCenter ]
        |> el Zed [ height fill, width fill ]


h1 : String -> Node
h1 label =
    Element.h1
        H1
        [ inlineStyle [ ( "display", "table" ) ]
        , moveUp 1
        , paddingBottom (scaled -17)
        , paddingLeft (scaled -4)
        , paddingRight (scaled -4)
        , paddingTop (scaled -10)
        ]
        (text label)


inputBottomPadding : Attr
inputBottomPadding =
    paddingBottom ((scaled -5) - 1)


inputTopPadding : Attr
inputTopPadding =
    paddingTop ((scaled -5) + 1)


intro : List Node -> Node
intro children =
    paragraph
        Intro
        [ paddingBottom (scaled 7), paddingTop (scaled 4) ]
        children


lbl : String -> Node
lbl theLabel =
    el (Form Label) [] (text theLabel)


listItem : List Attr -> List Node -> Node
listItem additionalAttr =
    row
        (List Item)
        ((++)
            [ paddingXY 0 (scaled -2), verticalCenter ]
            additionalAttr
        )


listItemActions : List Node -> Node
listItemActions =
    row
        WithoutLineHeight
        [ spacingXY (scaled -8) 0, verticalCenter ]


logoBackdrop : Node
logoBackdrop =
    el
        LogoBackdrop
        [ height fill, width fill ]
        empty


select : (String -> TopLevel.Msg) -> String -> List ( String, String ) -> Node
select onInputMsg selectedValue options =
    within
        [ 20
            |> Icons.expand_more (Color.greyscale 0.325)
            |> html
            |> el Zed
                [ alignRight
                , inlineStyle [ ( "line-height", "0" ), ( "pointer-events", "none" ) ]
                , verticalCenter
                ]
        ]
        (Html.select
            [ Html.Events.onInput onInputMsg
            , Html.Attributes.style
                [ ( "-moz-appearance", "none" )
                , ( "-webkit-appearance", "none" )
                , ( "appearance", "none" )
                , ( "background", "transparent" )
                , ( "border", "0" )
                , ( "color", Color.Convert.colorToCssRgb colorDerivatives.text )
                , ( "font-family", "Overpass, sans-serif" )
                , ( "font-size", (toString <| scaled 0) ++ "px" )
                , ( "padding", (toString <| scaled -3) ++ "px 0" )
                , ( "width", "100%" )
                ]
            ]
            (List.map
                (\( val, lbl ) ->
                    Html.option
                        [ Html.Attributes.selected (val == selectedValue)
                        , Html.Attributes.value val
                        ]
                        [ Html.text lbl ]
                )
                options
            )
            |> html
            |> el (Form Input) [ width fill ]
        )


takeOver : Node -> Node
takeOver =
    el Zed [ height fill, width fill ]
