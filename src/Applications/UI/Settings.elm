module UI.Settings exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Tachyons.Classes as T
import UI.Core
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page as Page



-- 🗺


view : UI.Core.Model -> List (Html UI.Core.Msg)
view =
    index



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
          , PerformMsg UI.Core.Bypass
          )
        ]
        model.page

    -----------------------------------------
    -- Content
    -----------------------------------------
    , UI.Kit.canister
        [ UI.Kit.h1 "Settings"
        , [ text "Changes are automatically saved."
          ]
            |> Html.span []
            |> UI.Kit.intro
        ]
    ]
