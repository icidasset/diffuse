module ContextMenu.View exposing (entry)

import ContextMenu.Styles exposing (Styles(..))
import ContextMenu.Types exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onWithOptions)
import Element.Types exposing (..)
import Json.Decode
import Mouse exposing (Position)
import Styles exposing (Styles(..))
import Svg exposing (Svg)
import Types as TopLevel exposing (Msg(ContextMenuMsg))
import Variables exposing (scaled)
import Variations exposing (Variations)


-- 🍯


entry : Maybe (ContextMenu TopLevel.Msg) -> Node
entry m =
    case m of
        Just (ContextMenu.Types.ContextMenu items mousePos) ->
            column
                (Styles.ContextMenu Container)
                (attributes mousePos)
                (List.map itemView items)

        _ ->
            empty



-- 🐝


attributes : Position -> List Attr
attributes mousePos =
    [ minWidth (px 170)

    -- Events
    , onWithOptions "click" eventOptions (Json.Decode.succeed <| ContextMenuMsg <| Hide)

    -- Position
    , inlineStyle
        [ ( "position", "absolute" )
        , ( "left", toString mousePos.x ++ "px" )
        , ( "top", toString mousePos.y ++ "px" )
        , ( "z-index", "1000" )
        ]
    ]


eventOptions : Element.Events.Options
eventOptions =
    { preventDefault = True
    , stopPropagation = True
    }


itemView : ( Svg TopLevel.Msg, String, TopLevel.Msg ) -> Node
itemView ( icon, label, msg ) =
    row
        (Styles.ContextMenu Item)
        [ paddingXY (scaled 1) (scaled -3)
        , paddingRight (scaled 6)
        , spacing (scaled -3)
        , verticalCenter

        -- Events
        , onClick msg
        ]
        [ el WithoutLineHeight [] (html icon)
        , text label
        ]
