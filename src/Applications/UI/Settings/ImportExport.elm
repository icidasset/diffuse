module UI.Settings.ImportExport exposing (view)

import Chunky exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Material.Icons.Alert as Icons
import Material.Icons.Navigation as Icons
import Tachyons.Classes as T
import UI.Kit exposing (ButtonType(..))
import UI.Navigation exposing (..)
import UI.Page
import UI.Reply exposing (Reply(..))
import UI.Settings.Page
import User.Layer exposing (Method(..))



-- 🗺


view : Maybe Method -> Html Reply
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
        , UI.Kit.canister
            [ UI.Kit.h1 "Import & Export"

            -- Intro
            --------
            , [ UI.Kit.inlineIcon Icons.warning
              , Html.strong [] [ text "All your data will be replaced when you import something." ]
              ]
                |> raw
                |> UI.Kit.intro

            -- Import
            ---------
            , chunk [ C.mb_2, T.mt4 ] [ UI.Kit.label [] "Import" ]
            , UI.Kit.buttonWithColor
                UI.Kit.Gray
                Normal
                RequestImport
                (text "Choose file")
            , case userLayerMethod of
                Just Blockstack ->
                    otherImportOptions

                Just Local ->
                    otherImportOptions

                Just (RemoteStorage _) ->
                    otherImportOptions

                _ ->
                    nothing

            -- Export
            ---------
            , chunk [ C.mb_2, T.mt4 ] [ UI.Kit.label [] "Export" ]
            , UI.Kit.button
                Normal
                Export
                (text "Export data")
            ]
        ]


otherImportOptions : Html Reply
otherImportOptions =
    raw
        [ chunk
            [ C.text_xs, T.i, T.lh_copy, C.mt_3 ]
            [ text "Other options:" ]
        , chunk
            [ T.f6, T.lh_copy, T.mt2 ]
            [ inline [ C.mr_2 ] [ text "•" ]
            , UI.Kit.textButton
                { label = "Import Diffuse V1 data"
                , onClick = ImportLegacyData
                }
            ]
        ]
