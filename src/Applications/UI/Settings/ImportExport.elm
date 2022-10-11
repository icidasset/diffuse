module UI.Settings.ImportExport exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Round as Icons
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
                    [ "font-semibold" ]
                    [ text "All your data will be replaced when you import something." ]
              ]
                |> raw
                |> UI.Kit.intro

            --
            , chunk
                [ "sm:flex", "sm:neg-mt-6" ]
                [ -- Import
                  ---------
                  chunk
                    [ "flex-auto", "pr-2" ]
                    [ chunk [ "mb-2", "mt-8" ] [ UI.Kit.label [] "Import" ]
                    , UI.Kit.buttonWithColor
                        UI.Kit.Gray
                        Normal
                        RequestImport
                        (text "Choose file")
                    ]

                -- Export
                ---------
                , chunk
                    [ "flex-auto", "pl-2" ]
                    [ chunk [ "mb-2", "mt-8" ] [ UI.Kit.label [] "Export" ]
                    , UI.Kit.button
                        Normal
                        Export
                        (text "Export data")

                    --
                    -- TODO: Sync with other storage
                    --
                    -- , chunk
                    --     [ "italic", "leading-normal", "mt-5", "text-xs" ]
                    --     [ text "Other options:" ]
                    -- , chunk
                    --     [ "leading-normal", "mt-2", "text-sm" ]
                    --     [ inline [ "mr-2" ] [ text "•" ]
                    --     , UI.Kit.textButton
                    --         { label = "Migrate to another storage"
                    --         , onClick = MigrateHypaethralUserData
                    --         }
                    --     ]
                    ]
                ]
            ]
        ]
