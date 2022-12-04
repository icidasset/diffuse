module UI.Settings.Data exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Html.Events.Extra.Mouse as Mouse
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import UI.Kit exposing (ButtonColor(..), ButtonType(..))
import UI.Navigation exposing (..)
import UI.Page
import UI.Settings.Page
import UI.Syncing.Types as Syncing
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..), methodName)



-- 🗺


view : Maybe Method -> Html Msg
view userLayerMethod =
    UI.Kit.receptacle
        { scrolling = True }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          UI.Navigation.local
            [ ( Icon Icons.arrow_back
              , Label "Settings" Hidden
              , NavigateToPage (UI.Page.Settings UI.Settings.Page.Index)
              )
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , chunk
            [ "relative" ]
            [ chunk
                [ "absolute", "left-0", "top-0" ]
                [ UI.Kit.canister [ UI.Kit.h1 "Data & Sync" ] ]
            ]

        --
        , UI.Kit.focusScreen
            { icon = Icons.account_circle
            , iconHref = Nothing
            , text =
                [ text "Store your playlists, favourites and other"
                , lineBreak
                , text " data in a location of your choice."
                ]
            , textHref = Nothing
            }
            [ chunk
                [ "flex", "space-x-2.5" ]
                [ UI.Kit.button
                    Normal
                    RequestImport
                    (text "Import data")

                --
                , case userLayerMethod of
                    Just method ->
                        UI.Kit.buttonWithColor
                            Accent
                            Filled
                            (SyncingMsg Syncing.StopSync)
                            (text <| "Stop syncing with " ++ methodName method)

                    Nothing ->
                        UI.Kit.buttonWithOptions
                            Html.button
                            [ Mouse.onClick (SyncingMsg << Syncing.ShowSyncDataMenu) ]
                            Accent
                            Filled
                            Nothing
                            (text "Choose location")

                --
                , UI.Kit.buttonWithColor
                    Gray
                    Filled
                    Export
                    (text "Export data")
                ]
            ]
        ]
