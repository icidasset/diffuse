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


view : UI.Core.Model -> Sources.Page -> List (Html UI.Core.Msg)
view model page =
    case page of
        Index ->
            index model

        New form ->
            new form model



-- INDEX


index : UI.Core.Model -> List (Html UI.Core.Msg)
index model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.add
          , Label "Add a new source" Shown
          , GoToPage (Page.Sources <| New newForm)
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



-- NEW


new : Form -> UI.Core.Model -> List (Html UI.Core.Msg)
new form =
    case form.step of
        Where ->
            newWhere form

        _ ->
            always [ empty ]


newWhere : Form -> UI.Core.Model -> List (Html UI.Core.Msg)
newWhere { context } model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Back to list" Hidden
          , GoToPage (Page.Sources Sources.Index)
          )
        ]
        model.page

    -----------------------------------------
    -- Content
    -----------------------------------------
    , chunk
        [ T.flex
        , T.flex_grow_1
        , T.items_center
        , T.justify_center
        , T.relative
        ]
        [ UI.Kit.h2 "Where is your music stored?"
        ]
    ]
