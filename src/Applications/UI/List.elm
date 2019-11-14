module UI.List exposing (Action, Item, Variant(..), view)

import Chunky exposing (..)
import Color
import Color.Ext as Color
import Conditional exposing (..)
import Css exposing (px, solid)
import Css.Classes as C
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style, title)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse exposing (onWithOptions)
import List.Ext as List
import Material.Icons exposing (Coloring(..))
import Maybe.Extra as Maybe
import Tachyons.Classes as T
import UI.DnD as DnD
import UI.Kit
import VirtualDom



-- 🌳


type alias Action msg =
    { color : Coloring
    , icon : Int -> Coloring -> VirtualDom.Node msg
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
    items
        |> List.indexedMap (item variant)
        |> brick [ style "font-size" "13p" ] [ T.lh_title ]



-- TODO
-----------------------------------------
-- ㊙️
-----------------------------------------


item : Variant Int msg -> Int -> Item msg -> Html msg
item variant idx { label, actions, msg, isSelected } =
    brick
        []
        -- TODO:
        -- (case variant of
        --     Normal ->
        --         [ { dragTarget = False
        --           , isSelected = isSelected
        --           }
        --             |> itemStyles
        --             |> css
        --         ]
        --
        --     Draggable env ->
        --         [ { dragTarget = DnD.isDraggingOver idx env.model
        --           , isSelected = isSelected
        --           }
        --             |> itemStyles
        --             |> css
        --         ]
        -- )
        [ T.flex
        , T.fw6
        , T.items_center

        --
        , ifThenElse (Maybe.isJust msg) T.pointer ""
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
            [ T.flex_grow_1, T.pv3, T.overflow_hidden ]
            [ label ]

        -- Actions
        ----------
        , chunk
            [ T.flex
            , T.items_center

            --
            , case variant of
                Normal ->
                    ""

                Draggable env ->
                    if DnD.isDragging env.model then
                        C.pointer_events_none

                    else
                        ""
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
        , T.ml1
        , T.pl1
        , ifThenElse (Maybe.isJust action.msg) T.pointer ""
        ]
        [ action.icon 16 action.color ]



-- 🖼


itemStyles : { dragTarget : Bool, isSelected : Bool } -> List Css.Style
itemStyles { dragTarget, isSelected } =
    if dragTarget then
        itemBaseStyles
            |> List.add [ Css.borderTop3 (px 1) solid (Color.toElmCssColor UI.Kit.colorKit.accent) ]
            |> List.add (ifThenElse isSelected [ Css.color selectionColor ] [])

    else if isSelected then
        List.append
            itemBaseStyles
            [ Css.color selectionColor
            ]

    else
        itemBaseStyles


itemBaseStyles : List Css.Style
itemBaseStyles =
    [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor UI.Kit.colors.verySubtleBorder)
    , Css.borderTop3 (px 1) solid Css.transparent
    , Css.marginTop (px -1)
    , Css.touchAction Css.none
    ]


selectionColor : Css.Color
selectionColor =
    Color.toElmCssColor UI.Kit.colors.selection
