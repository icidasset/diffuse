module UI.Sources.ContextMenu exposing (sourceMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.Image as Icons
import Sources exposing (Source)
import UI.Core exposing (Msg(..))
import UI.Page
import UI.Sources as Sources
import UI.Sources.Page



-- 🔱


sourceMenu : Source -> Coordinates -> ContextMenu Msg
sourceMenu source =
    ContextMenu
        [ ( Icons.edit
          , "Edit"
          , source.id
                |> UI.Sources.Page.Edit
                |> UI.Page.Sources
                |> ChangeUrlUsingPage
          )
        , ( Icons.delete
          , "Remove"
          , SourcesMsg (Sources.RemoveFromCollection { sourceId = source.id })
          )
        ]
