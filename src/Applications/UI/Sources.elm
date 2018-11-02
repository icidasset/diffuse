module UI.Sources exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Sources exposing (..)
import Tachyons.Classes as T
import UI.Core
import UI.Kit
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
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          UI.Navigation.local
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

        -----------------------------------------
        -- Content
        -----------------------------------------
        , UI.Kit.canister
            [ UI.Kit.h1 "Sources"
            , [ text "A source is a place where your music is stored."
              , lineBreak
              , text "By connecting a source, the application will scan it and keep a list of all the music in it."
              , lineBreak
              , text "It will not copy anything."
              ]
                |> Html.span []
                |> UI.Kit.intro
            ]
        ]
