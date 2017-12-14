module Queue.View exposing (..)

import Color
import Color.Convert exposing (colorToCssRgb)
import Html exposing (..)
import Html.Attributes exposing (rel, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy3)
import Html5.DragDrop as DragDrop
import Material.Icons.Action as Icons
import Material.Icons.Content as Icons
import Material.Icons.Image as Icons
import Material.Icons.Navigation as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Queue.Types as Queue exposing (Item, Page(..))
import Routing.Types
import Types as TopLevel exposing (Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colors)


-- Styles

import List.Styles exposing (Classes(..))
import StylesOld exposing (Classes(..))


-- Helpers


type alias ItemWithActions =
    ( Item, List (Html Msg) )



-- 🍯


entry : Queue.Page -> TopLevel.Model -> Html TopLevel.Msg
entry page model =
    case page of
        Index ->
            lazy3 pageIndex model.queue.future model.queue.shuffle model.queue.dnd

        History ->
            lazy pageHistory model.queue.past



-- {Page} index


pageIndex : List Item -> Bool -> DragDrop.Model Int Int -> Html TopLevel.Msg
pageIndex futureItems shuffled dnd =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            [ ( Icon Icons.arrow_back
              , Label (Hidden "Go back")
                --
              , Routing.Types.Index
                    |> Routing.Types.GoToPage
                    |> RoutingMsg
              )
            , ( Icon Icons.event_seat
              , Label (Shown "History")
                --
              , History
                    |> Routing.Types.Queue
                    |> Routing.Types.GoToPage
                    |> RoutingMsg
              )
            , ( Icon Icons.clear
              , Label (Shown "Clear all")
                --
              , QueueMsg (Queue.Clear)
              )
            , ( Icon Icons.clear
              , Label (Shown "Clear ignored")
                --
              , QueueMsg (Queue.Reset)
              )
            ]

        ------------------------------------
        -- List
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Up next" ]
            , if List.isEmpty futureItems then
                div
                    [ cssClass EmptyState ]
                    [ Icons.music_note Color.black 16
                    , div
                        []
                        [ text "Nothing here yet,"
                        , br [] []
                        , text "add some music first."
                        ]
                    ]
              else
                let
                    dropTarget =
                        DragDrop.getDropId dnd
                in
                    ul
                        [ cssClass ListWithActions ]
                        (futureItems
                            |> List.indexedMap (futureActions shuffled)
                            |> List.indexedMap (renderDraggableItem dropTarget)
                        )
            ]
        ]


futureActions : Bool -> Int -> Item -> ItemWithActions
futureActions _ index item =
    (,)
        item
        [ a
            [ index
                |> Queue.RemoveItem
                |> TopLevel.QueueMsg
                |> onClick
            ]
            [ Icons.remove_circle_outline (Color.grayscale 0.175) 16 ]
        ]



-- {Page} history


pageHistory : List Item -> Html TopLevel.Msg
pageHistory pastItems =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( Icon Icons.arrow_back
              , Label (Hidden "Go back")
              , Routing.Types.Index
              )
            , ( Icon Icons.event_seat
              , Label (Shown "Up next")
              , Routing.Types.Queue Index
              )
            ]

        ------------------------------------
        -- List
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "History" ]
            , if List.isEmpty pastItems then
                div
                    [ cssClass EmptyState ]
                    [ Icons.music_note Color.black 16
                    , div
                        []
                        [ text "Nothing here yet,"
                        , br [] []
                        , text "play some music first."
                        ]
                    ]
              else
                ul
                    [ cssClass ListWithActions ]
                    (pastItems
                        |> List.reverse
                        |> List.map (\item -> ( item, [] ))
                        |> List.indexedMap renderItem
                    )
            ]
        ]



-- Child views


renderItem : Int -> ItemWithActions -> Html TopLevel.Msg
renderItem index ( item, actions ) =
    li
        [ rel (toString index) ]
        [ itemLabel index item
        , span [ cssClass ListActions ] (actions)
        ]


renderDraggableItem : Maybe Int -> Int -> ItemWithActions -> Html TopLevel.Msg
renderDraggableItem maybeDropTarget index ( item, actions ) =
    let
        styles =
            case maybeDropTarget of
                Just dropTarget ->
                    if index == dropTarget then
                        dropTargetStyles
                    else
                        []

                Nothing ->
                    []
    in
        li
            ([ rel (toString index), cssClass DraggableListItem ]
                ++ DragDrop.draggable (TopLevel.QueueMsg << Queue.DragDropMsg) index
                ++ DragDrop.droppable (TopLevel.QueueMsg << Queue.DragDropMsg) index
                ++ List.singleton (style styles)
            )
            [ itemLabel index item
            , span [ cssClass ListActions ] (actions)
            ]


itemLabel : Int -> Item -> Html Msg
itemLabel index item =
    let
        track =
            Tuple.second item.identifiedTrack

        lbl =
            track.tags.artist ++ " – " ++ track.tags.title
    in
        label
            []
            [ small [] [ index + 1 |> toString |> text ]
            , if item.manualEntry == True then
                span [] [ text lbl ]
              else
                span [ cssClass SubtleListItem ] [ text lbl ]
            ]


dropTargetStyles : List ( String, String )
dropTargetStyles =
    [ ( "border-top", "1px solid " ++ colorToCssRgb colors.base06 ) ]
