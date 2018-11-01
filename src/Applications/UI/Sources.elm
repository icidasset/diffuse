module UI.Sources exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Sources exposing (..)
import UI.Core
import UI.Navigation exposing (..)
import UI.Page as Page



-- 🗺


view : UI.Core.Model -> Sources.Page -> Html UI.Core.Msg
view model page =
    case page of
        Index ->
            index model



-- PAGES


index : UI.Core.Model -> Html UI.Core.Msg
index model =
    chunk
        []
        [ UI.Navigation.local
            [ ( Icon Icons.add
              , Label "Add a new source" Shown
              , GoToPage (Page.Sources Sources.Index)
              )
            , ( Icon Icons.sync
              , Label "Process sources" Shown
              , PerformMsg UI.Core.Bypass
              )
            ]
            model.page
        ]
