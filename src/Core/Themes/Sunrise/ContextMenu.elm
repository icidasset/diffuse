module Themes.Sunrise.ContextMenu exposing (view)

import Chunky exposing (..)
import Conditional exposing (..)
import ContextMenu exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (custom)
import Json.Decode
import Material.Icons.Types exposing (Coloring(..))
import UI.Types as UI exposing (Msg)



-- 🗺


view : Float -> Maybe (ContextMenu Msg) -> Html Msg
view viewportWidth m =
    case m of
        Just (ContextMenu items coordinates) ->
            let
                ( height, width ) =
                    ( 250, 170 )

                left =
                    coordinates.x
                        |> max (width / 2 + 12)
                        |> min (viewportWidth - width / 2 - 12)

                top =
                    coordinates.y
                        |> max (height / 2 + 12)
            in
            brick
                [ style "left" (String.fromFloat left ++ "px")
                , style "top" (String.fromFloat top ++ "px")

                --
                , style "min-width" "170px"
                ]
                [ "absolute"
                , "bg-white"
                , "leading-loose"
                , "opacity-95"
                , "overflow-hidden"
                , "-translate-x-1/2"
                , "-translate-y-1/2"
                , "rounded"
                , "shadow-md"
                , "select-none"
                , "text-almost-sm"
                , "transform"
                , "z-50"

                -- Dark mode
                ------------
                , "dark:bg-darkest-hour"
                , "dark:border"
                , "dark:border-base00"
                ]
                (List.map
                    (\item ->
                        case item of
                            Item i ->
                                itemView i

                            Divider ->
                                -- NOTE: Not needed at the moment
                                nothing
                    )
                    items
                )

        Nothing ->
            nothing


itemView : ContextMenu.ItemProperties Msg -> Html Msg
itemView { icon, label, msg, active } =
    brick
        [ custom
            "tap"
            (Json.Decode.succeed
                { message = UI.MsgViaContextMenu msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
        ]
        [ "border-b"
        , "cursor-pointer"
        , "pl-4"
        , "pr-8"
        , "py-3"
        , "truncate"

        --
        , "last:border-transparent"

        --
        , ifThenElse active "antialiased" "subpixel-antialiased"
        , ifThenElse active "border-transparent" "border-gray-200"
        , ifThenElse active "bg-base00" "bg-transparent"
        , ifThenElse active "text-white" "text-inherit"
        , ifThenElse active "font-semibold" "font-normal"

        -- Dark mode
        ------------
        , "dark:border-base00"

        --
        , ifThenElse active "dark:bg-base07" "dark:bg-transparent"
        , ifThenElse active "dark:text-darkest-hour" "dark:text-inherit"
        ]
        [ inline
            [ "align-middle"
            , "inline-block"
            , "leading-0"
            ]
            [ icon 14 Inherit ]
        , inline
            [ "align-middle"
            , "inline-block"
            , "leading-0"
            , "ml-2"
            , "pl-1"
            , "relative"
            ]
            [ text label ]
        ]
