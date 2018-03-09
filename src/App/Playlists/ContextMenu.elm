module Playlists.ContextMenu exposing (..)

import Color
import Material.Icons.Action as Icons
import Material.Icons.Editor as Icons
import Mouse
import Playlists.Types exposing (..)
import Routing.Types
import Types exposing (..)
import Variables exposing (colorDerivatives)


listMenu : String -> Mouse.Position -> ContextMenu
listMenu playlistName =
    ContextMenu
        [ -- Edit
          --
          ( Icons.mode_edit colorDerivatives.text 16
          , "Edit"
          , playlistName
                |> Edit
                |> Routing.Types.Playlists
                |> Routing.Types.GoToPage
                |> RoutingMsg
          )

        -- Delete
        --
        , ( Icons.delete colorDerivatives.text 16
          , "Remove"
          , playlistName
                |> Remove
                |> PlaylistsMsg
          )
        ]
