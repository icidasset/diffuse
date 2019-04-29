module UI.Settings.ImportExport exposing (view)

import Chunky exposing (..)
import Html.Styled as Html exposing (Html, text)
import Material.Icons.Alert as Icons
import Material.Icons.Navigation as Icons
import Tachyons.Classes as T
import UI.Core
import UI.Kit exposing (ButtonType(..))
import UI.Navigation exposing (..)
import UI.Page
import UI.Settings.Page



-- 🗺


view : Html UI.Core.Msg
view =
    UI.Kit.receptacle
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
        , UI.Kit.canister
            [ UI.Kit.h1 "Import / Export"

            -- Intro
            --------
            , [ UI.Kit.inlineIcon Icons.warning
              , Html.strong [] [ text "All your data will be replaced when you import something." ]
              ]
                |> raw
                |> UI.Kit.intro

            -- Import
            ---------
            , chunk [ T.mb2, T.mt4 ] [ UI.Kit.label [] "Import" ]
            , UI.Kit.buttonWithColor
                UI.Kit.colorKit.base05
                Normal
                UI.Core.RequestImport
                (text "Choose file")

            -- Export
            ---------
            , chunk [ T.mb2, T.mt4 ] [ UI.Kit.label [] "Export" ]
            , UI.Kit.button
                Normal
                UI.Core.Export
                (text "Export data")
            ]
        ]
