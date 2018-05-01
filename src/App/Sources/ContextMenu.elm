module Sources.ContextMenu exposing (..)

import Color
import ContextMenu.Types exposing (..)
import Material.Icons.Action as Icons
import Material.Icons.Editor as Icons
import Mouse
import Routing.Types
import Sources.Types exposing (..)
import Types as TopLevel exposing (..)
import Variables exposing (colorDerivatives)


listMenu : SourceId -> Mouse.Position -> ContextMenu TopLevel.Msg
listMenu sourceId =
    ContextMenu
        [ -- Edit
          --
          ( Icons.mode_edit colorDerivatives.text 16
          , "Edit"
          , Edit sourceId
                |> Routing.Types.Sources
                |> Routing.Types.GoToPage
                |> RoutingMsg
          )

        -- Delete
        --
        , ( Icons.delete colorDerivatives.text 16
          , "Remove"
          , Destroy sourceId
                |> SourcesMsg
          )
        ]
