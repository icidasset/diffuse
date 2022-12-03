module UI.Settings.Syncing exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import UI.Kit exposing (ButtonType(..))
import UI.Navigation exposing (..)
import UI.Page
import UI.Settings.Page
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..))



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
                [ UI.Kit.canister [ UI.Kit.h1 "Syncing" ] ]
            ]

        --
        , UI.Kit.centeredContent
            [ slab
                Html.a
                [ href "(Page.toString <| Page.Sources New)" ]
                [ "block"
                , "opacity-30"
                , "text-inherit"
                ]
                [ Icons.sync_alt 64 Inherit ]
            , chunk
                [ "leading-normal"
                , "mt-2"
                , "opacity-40"
                , "text-center"
                , "text-inherit"
                ]
                [ text "Store your playlists, favourites and other"
                , lineBreak
                , text " data in a location of your choice."
                ]
            , chunk
                [ "mt-4" ]
                [ UI.Kit.button
                    Normal
                    Export
                    (text "Sync data")
                ]
            ]
        ]
