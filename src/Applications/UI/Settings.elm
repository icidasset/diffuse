module UI.Settings exposing (view)

import Authentication exposing (Method(..))
import Chunky exposing (..)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (selected, value)
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Tachyons.Classes as T
import UI.Authentication
import UI.Backdrop
import UI.Core
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page
import UI.Settings.ImportExport
import UI.Settings.Page as Settings exposing (..)
import UI.Tracks.Core



-- 🗺


view : Settings.Page -> UI.Core.Model -> Html UI.Core.Msg
view page model =
    case page of
        ImportExport ->
            UI.Settings.ImportExport.view

        Index ->
            UI.Kit.receptacle (index model)



-- PAGES


index : UI.Core.Model -> List (Html UI.Core.Msg)
index model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.import_export
          , Label "Import / Export" Shown
          , GoToPage (UI.Page.Settings ImportExport)
          )
        , ( Icon Icons.exit_to_app
          , Label "Sign out" Shown
          , PerformMsg UI.Core.SignOut
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , UI.Kit.canister
        [ UI.Kit.h1 "Settings"
        , [ text "Changes are saved automatically."
          , lineBreak
          , text "PS. You're storing the data for this application "
          , case UI.Authentication.extractMethod model.authentication of
                Just Ipfs ->
                    inline
                        []
                        [ text "on IPFS."
                        , lineBreak
                        , text "If you want to, you can "
                        , UI.Kit.textButton
                            { label = "change your passphrase"
                            , onClick =
                                Ipfs
                                    |> UI.Authentication.ShowUpdateEncryptionKeyScreen
                                    |> UI.Core.AuthenticationMsg
                            }
                        , text "."
                        ]

                Just Local ->
                    text "in this browser."

                Nothing ->
                    text "on nothing, wtf?"
          ]
            |> raw
            |> UI.Kit.intro

        -- Background image
        -------------------
        , chunk [ T.mb2, T.mt4 ] [ UI.Kit.label [] "Background Image" ]
        , UI.Kit.select
            (UI.Core.BackdropMsg << UI.Backdrop.Choose)
            (List.map
                (\( v, l ) ->
                    Html.option
                        [ selected (Just v == model.backdrop.chosen), value v ]
                        [ text l ]
                )
                UI.Backdrop.options
            )

        -- Duplicates
        -------------
        , chunk [ T.mb3, T.mt4 ] [ UI.Kit.label [] "Hide Duplicates" ]
        , UI.Kit.checkbox
            { checked = model.tracks.hideDuplicates
            , toggleMsg = UI.Core.TracksMsg UI.Tracks.Core.ToggleHideDuplicates
            }
        ]
    ]
