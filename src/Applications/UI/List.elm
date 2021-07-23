module UI.List exposing (Action, Item, Variant(..), view)

import Chunky exposing (..)
import Conditional exposing (..)
import Css.Classes as C
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
        [ C.antialiased, C.font_semibold, C.leading_snug, C.text_nearly_sm ]
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
        [ C.border_t

        --
        , if dragTarget then
            C.border_base03

          else
            C.border_transparent

        -- Dark mode
        ------------
        , if dragTarget then
            C.dark__border_gray_300

          else
            C.dark__border_transparent
        ]
    <|
        chunk
            [ C.border_b
            , C.border_gray_200
            , C.flex
            , C.items_center

            --
            , ifThenElse (Maybe.isJust msg) C.cursor_pointer C.cursor_default
            , ifThenElse isSelected C.text_base03 C.text_inherit

            -- Dark mode
            ------------
            , C.dark__border_base00
            , ifThenElse isSelected C.dark__text_gray_300 C.dark__text_inherit
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
                [ C.flex_grow
                , C.py_4
                , C.overflow_hidden
                ]
                [ label ]

            -- Actions
            ----------
            , chunk
                [ C.flex
                , C.items_center

                --
                , case variant of
                    Normal ->
                        C.pointer_events_auto

                    Draggable env ->
                        if DnD.isDragging env.model then
                            C.pointer_events_none

                        else
                            C.pointer_events_auto
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
        [ C.leading_0
        , C.ml_1
        , C.pl_1
        , ifThenElse (Maybe.isJust action.msg) C.cursor_pointer C.cursor_default
        ]
        [ action.icon 16 Inherit ]
