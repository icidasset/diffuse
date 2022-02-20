module UI.Queue.View exposing (view)

import Chunky exposing (..)
import Common
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Html.Lazy as Lazy
import Icons
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Queue exposing (..)
import UI.DnD as DnD
import UI.Kit
import UI.List
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Queue.Page as Queue exposing (Page(..))
import UI.Queue.Types exposing (..)
import UI.Sources.Page
import UI.Types as UI exposing (..)



-- 🗺


view : Queue.Page -> UI.Model -> Html UI.Msg
view page model =
    case page of
        History ->
            Lazy.lazy2
                historyView
                model.playedPreviously
                model.dnd

        Index ->
            Lazy.lazy3
                futureView
                model.playingNext
                model.selectedQueueItem
                model.dnd



-- 🗺  ░░  FUTURE


futureView : List Queue.Item -> Maybe Queue.Item -> DnD.Model Int -> Html UI.Msg
futureView playingNext selectedQueueItem dnd =
    UI.Kit.receptacle
        { scrolling = not (DnD.isDragging dnd) }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          UI.Navigation.local
            [ ( Icon Icons.arrow_back
              , Label Common.backToIndex Hidden
              , NavigateToPage Page.Index
              )
            , ( Icon Icons.history
              , Label "History" Shown
              , NavigateToPage (Page.Queue History)
              )
            , ( Icon Icons.clear
              , Label "Clear" Shown
              , PerformMsg (QueueMsg Clear)
              )
            , ( Icon Icons.not_interested
              , Label "Reset ignored" Shown
              , PerformMsg (QueueMsg Reset)
              )
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , if List.isEmpty playingNext then
            chunk
                [ "relative" ]
                [ chunk
                    [ "absolute", "left-0", "top-0" ]
                    [ UI.Kit.canister [ UI.Kit.h1 "Up next" ] ]
                ]

          else
            UI.Kit.canister
                [ UI.Kit.h1 "Up next"
                , playingNext
                    |> List.indexedMap (futureItem selectedQueueItem)
                    |> UI.List.view
                        (UI.List.Draggable
                            { model = dnd
                            , toMsg = UI.DnD
                            }
                        )
                    |> chunky [ "mt-3" ]
                ]

        --
        , if List.isEmpty playingNext then
            UI.Kit.centeredContent
                [ slab
                    Html.a
                    [ href (Page.toString <| Page.Sources UI.Sources.Page.New) ]
                    [ "text-inherit", "block", "opacity-30" ]
                    [ Icons.music_note 64 Inherit ]
                , slab
                    Html.a
                    [ href (Page.toString <| Page.Sources UI.Sources.Page.New) ]
                    [ "text-inherit", "block", "leading-normal", "mt-2", "opacity-40", "text-center" ]
                    [ text "Nothing here yet,"
                    , lineBreak
                    , text "add some music first."
                    ]
                ]

          else
            nothing
        ]


futureItem : Maybe Item -> Int -> Queue.Item -> UI.List.Item UI.Msg
futureItem selectedQueueItem idx item =
    let
        ( identifiers, track ) =
            item.identifiedTrack

        isSelected =
            selectedQueueItem
                |> Maybe.map (.identifiedTrack >> Tuple.first >> .indexInList)
                |> (==) (Just identifiers.indexInList)

        iconFn =
            if item.manualEntry then
                identity

            else
                Icons.wrapped subtleFutureIconClasses
    in
    { label =
        inline
            [ "block"
            , "truncate"

            --
            , if item.manualEntry || isSelected then
                "text-inherit"

              else
                "text-base05"

            -- Dark mode
            ------------
            , if item.manualEntry || isSelected then
                "dark:text-inherit"

              else
                "dark:text-base04"
            ]
            [ inline
                [ "inline-block"
                , "mr-2"
                , "opacity-60"
                , "text-xs"
                ]
                [ text (String.fromInt <| idx + 1), text "." ]
            , text (track.tags.artist ++ " - " ++ track.tags.title)
            ]
    , actions =
        [ -- Remove
          ---------
          { icon =
                if item.manualEntry then
                    iconFn Icons.remove_circle_outline

                else
                    iconFn Icons.not_interested
          , msg =
                { index = idx, item = item }
                    |> RemoveItem
                    |> QueueMsg
                    |> always
                    |> Just
          , title =
                ifThenElse item.manualEntry "Remove" "Ignore"
          }

        -- Menu
        -------
        , { icon =
                iconFn Icons.more_vert
          , msg =
                Just (QueueMsg << ShowFutureMenu item { index = idx })
          , title =
                "Menu"
          }
        ]
    , msg = Just (QueueMsg <| Select item)
    , isSelected = isSelected
    }


subtleFutureIconClasses : List String
subtleFutureIconClasses =
    [ "text-gray-500"

    -- Dark mode
    ------------
    , "dark:text-base02"
    ]



-- 🗺  ░░  HISTORY


historyView : List Queue.Item -> DnD.Model Int -> Html UI.Msg
historyView playedPreviously dnd =
    UI.Kit.receptacle
        { scrolling = not (DnD.isDragging dnd) }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          UI.Navigation.local
            [ ( Icon Icons.arrow_back
              , Label Common.backToIndex Hidden
              , NavigateToPage Page.Index
              )
            , ( Icon Icons.update
              , Label "Up next" Shown
              , NavigateToPage (Page.Queue Index)
              )
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , if List.isEmpty playedPreviously then
            chunk
                [ "relative" ]
                [ chunk
                    [ "absolute", "left-0", "top-0" ]
                    [ UI.Kit.canister [ UI.Kit.h1 "History" ] ]
                ]

          else
            UI.Kit.canister
                [ UI.Kit.h1 "History"
                , playedPreviously
                    |> List.reverse
                    |> List.indexedMap historyItem
                    |> UI.List.view UI.List.Normal
                    |> chunky [ "mt-3" ]
                ]

        --
        , if List.isEmpty playedPreviously then
            UI.Kit.centeredContent
                [ chunk
                    [ "opacity-30" ]
                    [ Icons.music_note 64 Inherit ]
                , chunk
                    [ "leading-normal", "mt-2", "opacity-40", "text-center" ]
                    [ text "Nothing here yet,"
                    , lineBreak
                    , text "play some music first."
                    ]
                ]

          else
            nothing
        ]


historyItem : Int -> Queue.Item -> UI.List.Item UI.Msg
historyItem idx ({ identifiedTrack } as item) =
    let
        ( _, track ) =
            identifiedTrack
    in
    { label =
        inline
            [ "block", "truncate" ]
            [ inline
                [ "inline-block", "text-xs", "mr-2" ]
                [ text (String.fromInt <| idx + 1), text "." ]
            , text (track.tags.artist ++ " - " ++ track.tags.title)
            ]
    , actions =
        [ { icon = Icons.more_vert
          , msg = Just (QueueMsg << ShowHistoryMenu item)
          , title = "Menu"
          }
        ]
    , msg = Nothing
    , isSelected = False
    }
