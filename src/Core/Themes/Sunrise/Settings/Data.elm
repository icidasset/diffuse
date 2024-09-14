module Themes.Sunrise.Settings.Data exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Round as Icons
import Themes.Sunrise.Kit as Kit exposing (ButtonColor(..), ButtonType(..))
import Themes.Sunrise.Navigation as Navigation
import Themes.Sunrise.Settings.Common exposing (changePassphrase)
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Settings.Page exposing (Page(..))
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..))



-- 🗺


view : Maybe Method -> Html Msg
view activeMethod =
    Kit.receptacle
        { scrolling = True }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          (case activeMethod of
            Just (Dropbox d) ->
                [ changePassphrase (Dropbox d) ]

            Just (Ipfs i) ->
                [ changePassphrase (Ipfs i) ]

            Just (RemoteStorage r) ->
                [ changePassphrase (RemoteStorage r) ]

            Nothing ->
                []
          )
            |> List.append
                [ ( Icon Icons.arrow_back
                  , Label "Settings" Hidden
                  , NavigateToPage (Page.Settings Index)
                  )
                , ( Icon Icons.account_circle
                  , Label "Storage Service" Shown
                  , NavigateToPage (Page.Settings Sync)
                  )
                ]
            |> Navigation.local

        -----------------------------------------
        -- Content
        -----------------------------------------
        , chunk
            [ "relative" ]
            [ chunk
                [ "absolute", "left-0", "top-0" ]
                [ Kit.canister [ Kit.h1 "Data Backup" ] ]
            ]

        --
        , Kit.focusScreen
            { icon = Icons.archive
            , iconHref = Nothing
            , text =
                "You can download a snapshot of your user data, or recover an account by uploading a snapshot."
                    |> text
                    |> List.singleton
                    |> chunk [ "max-w-sm" ]
                    |> List.singleton
            , textHref = Nothing
            }
            [ chunk
                [ "flex", "space-x-2.5" ]
                [ Kit.button
                    Normal
                    RequestImport
                    (text "Import snapshot")

                --
                , Kit.buttonWithColor
                    Accent
                    Filled
                    Export
                    (text "Download snapshot")
                ]
            ]
        ]
