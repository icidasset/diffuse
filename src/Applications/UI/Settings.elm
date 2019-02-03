module UI.Settings exposing (view)

import Authentication exposing (Method(..))
import Chunky exposing (..)
import Html.Styled as Html exposing (Html, text)
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Tachyons.Classes as T
import UI.Core
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page as Page



-- 🗺


view : UI.Core.Model -> Html UI.Core.Msg
view =
    UI.Kit.receptacle << index



-- PAGES


index : UI.Core.Model -> List (Html UI.Core.Msg)
index model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.import_export
          , Label "Import / Export" Shown
          , PerformMsg UI.Core.Bypass
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
          , case model.authentication.methodInUse of
                Just Ipfs ->
                    text "on IPFS."

                Just Local ->
                    text "in this browser."

                Nothing ->
                    text "on nothing, wtf?"
          ]
            |> raw
            |> UI.Kit.intro
        ]
    ]
