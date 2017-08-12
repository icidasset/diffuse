module Sources.ContextMenu exposing (..)

import Color
import Material.Icons.Action as Icons
import Material.Icons.Editor as Icons
import Mouse
import Routing.Types
import Sources.Types exposing (..)
import Types exposing (..)


listMenu : SourceId -> Mouse.Position -> ContextMenu
listMenu sourceId =
    ContextMenu
        [ -- Edit
          --
          ( Icons.mode_edit (Color.black) 16
          , "Edit"
          , Edit sourceId
                |> Routing.Types.Sources
                |> Routing.Types.GoToPage
                |> RoutingMsg
          )

        -- Delete
        --
        , ( Icons.delete (Color.black) 16
          , "Remove"
          , Destroy sourceId
                |> SourcesMsg
          )
        ]
