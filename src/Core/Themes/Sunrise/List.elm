module Themes.Sunrise.List exposing (Action, Item, Variant(..), view)

import Chunky exposing (..)
import Conditional exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (title)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse exposing (onWithOptions)
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import UI.DnD as DnD
import VirtualDom



-- 🌳


type alias Action msg =
    { icon : Int -> Coloring -> VirtualDom.Node msg
    , msg : Maybe (Mouse.Event -> msg)
    , title : String
    }


type alias Item msg =
    { label : Html msg
    , actions : List (Action msg)
    , msg : Maybe msg
    , isSelected : Bool
    }


type Variant context msg
    = Normal
    | Draggable (DnD.Environment context msg)



-- ⛩


view : Variant Int msg -> List (Item msg) -> Html msg
view variant items =
    chunk
        [ "antialiased", "font-semibold", "leading-snug", "text-nearly-sm" ]
        (List.indexedMap (item variant) items)



-----------------------------------------
-- ㊙️
-----------------------------------------


item : Variant Int msg -> Int -> Item msg -> Html msg
item variant idx { label, actions, msg, isSelected } =
    let
        dragTarget =
            case variant of
                Normal ->
                    False

                Draggable env ->
                    DnD.isDraggingOver idx env.model
    in
    chunky
        [ "border-t"

        --
        , if dragTarget then
            "border-base03"

          else
            "border-transparent"

        -- Dark mode
        ------------
        , if dragTarget then
            "dark:border-gray-300"

          else
            "dark:border-transparent"
        ]
    <|
        chunk
            [ "border-b"
            , "border-gray-200"
            , "flex"
            , "items-center"

            --
            , ifThenElse (Maybe.isJust msg) "cursor-pointer" "cursor-default"
            , ifThenElse isSelected "text-base03" "text-inherit"

            -- Dark mode
            ------------
            , "dark:border-base00"
            , ifThenElse isSelected "dark:text-gray-300" "dark:text-inherit"
            ]
            [ -- Label
              --------
              brick
                (case variant of
                    Normal ->
                        case msg of
                            Just m ->
                                [ onClick m ]

                            Nothing ->
                                []

                    Draggable env ->
                        List.append
                            (case ( isSelected, msg ) of
                                ( True, _ ) ->
                                    [ DnD.listenToStart env idx ]

                                ( False, Just m ) ->
                                    [ onClick m ]

                                ( False, Nothing ) ->
                                    []
                            )
                            (DnD.listenToEnterLeave env idx)
                )
                [ "flex-grow"
                , "py-4"
                , "overflow-hidden"
                ]
                [ label ]

            -- Actions
            ----------
            , chunk
                [ "flex"
                , "items-center"

                --
                , case variant of
                    Normal ->
                        "pointer-events-auto"

                    Draggable env ->
                        if DnD.isDragging env.model then
                            "pointer-events-none"

                        else
                            "pointer-events-auto"
                ]
                (List.map actionView actions)
            ]


actionView : Action msg -> Html msg
actionView action =
    brick
        (case action.msg of
            Just msg ->
                [ title action.title

                --
                , onWithOptions "click" { stopPropagation = True, preventDefault = True } msg
                ]

            Nothing ->
                [ title action.title ]
        )
        [ "leading-0"
        , "ml-1"
        , "pl-1"
        , ifThenElse (Maybe.isJust action.msg) "cursor-pointer" "cursor-default"
        ]
        [ action.icon 16 Inherit ]
