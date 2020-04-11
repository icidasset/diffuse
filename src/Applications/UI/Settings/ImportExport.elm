module UI.Settings.ImportExport exposing (view)

import Chunky exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Material.Icons as Icons
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
        , UI.Kit.canister
            [ UI.Kit.h1 "Import & Export"

            -- Intro
            --------
            , [ UI.Kit.inlineIcon Icons.warning
              , inline
                    [ C.font_semibold ]
                    [ text "All your data will be replaced when you import something." ]
              ]
                |> raw
                |> UI.Kit.intro

            -- Import
            ---------
            , chunk [ C.mb_2, C.mt_8 ] [ UI.Kit.label [] "Import" ]
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
            , chunk [ C.mb_2, C.mt_8 ] [ UI.Kit.label [] "Export" ]
            , UI.Kit.button
                Normal
                Export
                (text "Export data")
            ]
        ]


otherImportOptions : Html Msg
otherImportOptions =
    raw
        [ chunk
            [ C.italic, C.leading_normal, C.mt_5, C.text_xs ]
            [ text "Other options:" ]
        , chunk
            [ C.leading_normal, C.mt_2, C.text_sm ]
            [ inline [ C.mr_2 ] [ text "•" ]
            , UI.Kit.textButton
                { label = "Import Diffuse V1 data"
                , onClick = ImportLegacyData
                }
            ]
        ]
