module Queue.View exposing (..)

import Color
import DnD
import Material.Icons.Action as Icons
import Material.Icons.Content as Icons
import Material.Icons.Image as Icons
import Material.Icons.Navigation as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Queue.Types as Queue exposing (Msg(DragItemMsg), Page(..))
import Routing.Types
import Types as TopLevel exposing (Msg(..))
import Variables exposing (colors)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Ext exposing (..)
import Element.Input as Input
import Element.Types exposing (Node)
import Layouts exposing (listItemActions)
import Variables exposing (scaled)
import Variations exposing (Variations(..))


-- Styles

import List.Styles exposing (Styles(..))
import Styles exposing (Styles(..))


-- Helpers


type alias ItemWithActions =
    ( Queue.Item, List Node )



-- 🍯


entry : Queue.Page -> TopLevel.Model -> Node
entry page model =
    case page of
        Index ->
            -- TODO: Use Element.Lazy once it's available
            pageIndex model.queue.future model.queue.shuffle model.queue.itemDnD

        History ->
            -- TODO: Use Element.Lazy once it's available
            pageHistory model.queue.past



-- {Page} index


pageIndex : List Queue.Item -> Bool -> DnD.Model Int -> Node
pageIndex futureItems shuffled itemDnD =
    column
        Zed
        [ height fill ]
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
        , column
            Zed
            [ height fill, paddingXY (scaled 4) 0 ]
            [ Layouts.h1 "Up next"

            --
            , if List.isEmpty futureItems then
                Layouts.emptyState
                    Icons.music_note
                    [ text "Nothing here yet,"
                    , text "add some music first."
                    ]
              else
                column
                    (List Container)
                    (List.append
                        [ paddingTop (scaled 1) ]
                        (List.map
                            (Element.Attributes.map QueueMsg)
                            (DnD.containerHooks DragItemMsg)
                        )
                    )
                    (futureItems
                        |> List.indexedMap (futureActions shuffled)
                        |> List.indexedMap (renderDraggableItem <| DnD.overSubject itemDnD)
                    )
            ]
        ]


futureActions : Bool -> Int -> Queue.Item -> ItemWithActions
futureActions _ index item =
    (,)
        item
        [ el
            WithoutLineHeight
            [ index
                |> Queue.RemoveItem
                |> TopLevel.QueueMsg
                |> onClick
            ]
            (16
                |> Icons.remove_circle_outline (Color.grayscale 0.175)
                |> html
            )
        ]



-- {Page} history


pageHistory : List Queue.Item -> Node
pageHistory pastItems =
    column
        Zed
        [ height fill ]
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
        , column
            Zed
            [ height fill, paddingXY (scaled 4) 0 ]
            [ Layouts.h1 "History"

            --
            , if List.isEmpty pastItems then
                Layouts.emptyState
                    Icons.music_note
                    [ text "Nothing here yet,"
                    , text "play some music first."
                    ]
              else
                column
                    (List Container)
                    [ paddingTop (scaled 1) ]
                    (pastItems
                        |> List.reverse
                        |> List.map (\item -> ( item, [] ))
                        |> List.indexedMap renderItem
                    )
            ]
        ]



-- Child views


renderItem : Int -> ItemWithActions -> Node
renderItem index ( item, actions ) =
    Layouts.listItem
        [ attribute "rel" (toString index)
        , vary Subtle (not item.manualEntry)
        ]
        [ itemLabel index item
        , listItemActions actions
        ]


renderDraggableItem : Maybe Int -> Int -> ItemWithActions -> Node
renderDraggableItem draggingOver index ( item, actions ) =
    let
        isDraggingOver =
            draggingOver
                |> Maybe.map ((==) index)
                |> Maybe.withDefault False

        attrs =
            [ attribute "rel" (toString index)
            , attribute "touch-action" "none"
            , vary Draggable True
            , vary DraggingOver isDraggingOver
            , vary Subtle (not item.manualEntry)
            ]
    in
        Layouts.listItem
            (List.append
                (index
                    |> DnD.itemHooks DragItemMsg
                    |> List.map (Element.Attributes.map QueueMsg)
                )
                attrs
            )
            [ itemLabel index item
            , listItemActions actions
            ]


itemLabel : Int -> Queue.Item -> Node
itemLabel index item =
    let
        track =
            Tuple.second item.identifiedTrack

        lbl =
            track.tags.artist ++ " – " ++ track.tags.title
    in
        row
            Zed
            [ width fill ]
            [ el
                (List Prefix)
                [ paddingRight (scaled 1) ]
                (index
                    |> (+) 1
                    |> toString
                    |> (\s -> s ++ ".")
                    |> text
                )

            --
            , el Zed [ width fill ] (text lbl)
            ]
