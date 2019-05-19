module UI.List exposing (Action, Item, Variant(..), view)

import Chunky exposing (..)
import Classes as C
import Color exposing (Color)
import Color.Ext as Color
import Conditional exposing (..)
import Css exposing (px, solid)
import Html.Events.Extra.Mouse as Mouse exposing (onWithOptions)
import Html.Styled as Html exposing (Html, fromUnstyled)
import Html.Styled.Attributes as Attributes exposing (css, style, title)
import Html.Styled.Events exposing (onClick)
import Material.Icons exposing (Coloring(..))
import Material.Icons.Action as Icons
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
    }


type Variant context msg
    = Normal
    | Draggable (DnD.Environment context msg)



-- ⛩


view : Variant Int msg -> List (Item msg) -> Html msg
view variant =
    List.indexedMap (item variant) >> brick [ css listStyles ] [ T.lh_title ]



-----------------------------------------
-- ㊙️
-----------------------------------------


item : Variant Int msg -> Int -> Item msg -> Html msg
item variant idx { label, actions, msg } =
    let
        dragHandleColoring =
            actions
                |> List.head
                |> Maybe.map .color
                |> Maybe.withDefault Inherit
    in
    brick
        (case variant of
            Normal ->
                List.append
                    [ css (itemStyles { dragTarget = False }) ]
                    (case msg of
                        Just m ->
                            [ onClick m ]

                        Nothing ->
                            []
                    )

            Draggable env ->
                List.concat
                    [ [ css (itemStyles { dragTarget = DnD.environmentTarget env == Just idx }) ]
                    , DnD.listenToEnterLeave env idx
                    , DnD.listenToDrop env idx
                    ]
        )
        [ T.flex
        , T.fw6
        , T.items_center
        , T.pv3

        --
        , ifThenElse (Maybe.isJust msg) T.pointer ""
        ]
        [ -- Label
          --------
          chunk
            [ T.flex_grow_1 ]
            [ label ]

        -- Actions
        ----------
        , chunk
            [ T.flex, T.items_center ]
            (List.append
                (List.map actionView actions)
                (case variant of
                    Normal ->
                        []

                    Draggable env ->
                        [ dragActionView dragHandleColoring env idx ]
                )
            )
        ]


actionView : Action msg -> Html msg
actionView action =
    brick
        (case action.msg of
            Just msg ->
                [ title action.title

                --
                , msg
                    |> onWithOptions "click" { stopPropagation = True, preventDefault = True }
                    |> Attributes.fromUnstyled
                ]

            Nothing ->
                [ title action.title ]
        )
        [ C.lh_0
        , T.ml2
        , ifThenElse (Maybe.isJust action.msg) T.pointer ""
        ]
        [ fromUnstyled (action.icon 16 action.color) ]


dragActionView : Coloring -> DnD.Environment Int msg -> Int -> Html msg
dragActionView coloring env context =
    brick
        [ title "Drag me"
        , DnD.listenToStart env context
        ]
        [ C.lh_0
        , C.grab_cursor
        , T.ml2
        ]
        [ fromUnstyled (Icons.drag_indicator 16 coloring) ]



-- 🖼


listStyles : List Css.Style
listStyles =
    [ Css.fontSize (px 13) ]


itemStyles : { dragTarget : Bool } -> List Css.Style
itemStyles { dragTarget } =
    if dragTarget then
        List.append
            itemBaseStyles
            [ Css.borderTop3 (px 1) solid (Color.toElmCssColor UI.Kit.colorKit.accent) ]

    else
        itemBaseStyles


itemBaseStyles : List Css.Style
itemBaseStyles =
    [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor UI.Kit.colors.verySubtleBorder)
    , Css.borderTop3 (px 1) solid Css.transparent
    , Css.marginTop (px -1)
    ]
