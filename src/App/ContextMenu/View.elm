module ContextMenu.View exposing (entry)

import ContextMenu.Styles exposing (Styles(..))
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onWithOptions)
import Json.Decode
import Mouse exposing (Position)
import Styles
import Svg exposing (Svg)
import Types exposing (ContextMenu, Msg(..))
import Variations exposing (Variations)


-- 🍯


entry : Maybe Types.ContextMenu -> Element Styles.Styles Variations Msg
entry m =
    case m of
        Just (Types.ContextMenu items mousePos) ->
            column
                (Styles.ContextMenu Container)
                (attributes mousePos)
                (List.map itemView items)

        _ ->
            empty



-- 🐝


attributes : Position -> List (Element.Attribute Variations Msg)
attributes mousePos =
    [ minWidth (px 170)

    -- Events
    , onWithOptions "click" eventOptions (Json.Decode.succeed NoOp)

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


itemView : ( Svg Msg, String, Msg ) -> Element Styles.Styles Variations Msg
itemView ( icon, label, msg ) =
    row
        (Styles.ContextMenu Item)
        [ HideContextMenu
            |> List.singleton
            |> List.append [ msg ]
            |> DoAll
            |> onClick
        ]
        [ html icon
        , text label
        ]
