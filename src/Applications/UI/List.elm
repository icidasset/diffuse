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
    , isSelected : Bool
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
item variant idx { label, actions, msg, isSelected } =
    brick
        (case variant of
            Normal ->
                List.append
                    [ { dragTarget = False
                      , isSelected = isSelected
                      }
                        |> itemStyles
                        |> css
                    ]
                    (case msg of
                        Just m ->
                            [ onClick m ]

                        Nothing ->
                            []
                    )

            Draggable env ->
                DnD.listenToEnterLeave env idx
                    |> List.map Attributes.fromUnstyled
                    |> List.append
                        (case ( isSelected, msg ) of
                            ( True, _ ) ->
                                [ Attributes.fromUnstyled (DnD.listenToStart env idx) ]

                            ( False, Just m ) ->
                                [ onClick m ]

                            ( False, Nothing ) ->
                                []
                        )
                    |> List.append
                        [ { dragTarget = DnD.isDraggingOver idx env.model
                          , isSelected = isSelected
                          }
                            |> itemStyles
                            |> css
                        ]
        )
        [ T.flex
        , T.fw6
        , T.items_center

        --
        , ifThenElse (Maybe.isJust msg) T.pointer ""
        ]
        [ -- Label
          --------
          chunk
            [ C.pointer_events_none, T.flex_grow_1, T.mv3, T.overflow_hidden ]
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
                , msg
                    |> onWithOptions "click" { stopPropagation = True, preventDefault = True }
                    |> Attributes.fromUnstyled
                ]

            Nothing ->
                [ title action.title ]
        )
        [ C.lh_0
        , T.ml1
        , T.pl1
        , ifThenElse (Maybe.isJust action.msg) T.pointer ""
        ]
        [ fromUnstyled (action.icon 16 action.color) ]



-- 🖼


listStyles : List Css.Style
listStyles =
    [ Css.fontSize (px 13) ]


itemStyles : { dragTarget : Bool, isSelected : Bool } -> List Css.Style
itemStyles { dragTarget, isSelected } =
    if dragTarget then
        List.append
            itemBaseStyles
            [ Css.borderTop3 (px 1) solid (Color.toElmCssColor UI.Kit.colorKit.accent)
            ]

    else if isSelected then
        List.append
            itemBaseStyles
            [ Css.color (Color.toElmCssColor UI.Kit.colors.selectionAlt)
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
