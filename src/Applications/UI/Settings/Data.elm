module UI.Settings.Data exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import UI.Kit exposing (ButtonColor(..), ButtonType(..))
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Settings.Page exposing (Page(..))
import UI.Types exposing (Msg(..))



-- 🗺


view : Html Msg
view =
    UI.Kit.receptacle
        { scrolling = True }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          UI.Navigation.local
            [ ( Icon Icons.arrow_back
              , Label "Settings" Hidden
              , NavigateToPage (Page.Settings Index)
              )
            , ( Icon Icons.account_circle
              , Label "Storage Service" Shown
              , NavigateToPage (Page.Settings Sync)
              )
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , chunk
            [ "relative" ]
            [ chunk
                [ "absolute", "left-0", "top-0" ]
                [ UI.Kit.canister [ UI.Kit.h1 "Data Backup" ] ]
            ]

        --
        , UI.Kit.focusScreen
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
                [ UI.Kit.button
                    Normal
                    RequestImport
                    (text "Import snapshot")

                --
                , UI.Kit.buttonWithColor
                    Accent
                    Filled
                    Export
                    (text "Download snapshot")
                ]
            ]
        ]
