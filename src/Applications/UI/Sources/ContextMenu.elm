module UI.Sources.ContextMenu exposing (sourceMenu)

import Conditional exposing (ifThenElse)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.File as Icons
import Material.Icons.Image as Icons
import Material.Icons.Notification as Icons
import Sources exposing (Source)
import UI.Page
import UI.Reply exposing (Reply(..))
import UI.Sources as Sources
import UI.Sources.Page



-- 🔱


sourceMenu : Source -> Coordinates -> ContextMenu Reply
sourceMenu source =
    ContextMenu
        [ Item
            { icon = ifThenElse source.directoryPlaylists Icons.folder Icons.folder_open
            , label = ifThenElse source.directoryPlaylists "Disable Directory Playlists" "Enable Directory Playlists"
            , msg = ToggleDirectoryPlaylists { sourceId = source.id }
            , active = False
            }
        , Item
            { icon = Icons.edit
            , label = "Edit source"
            , msg =
                source.id
                    |> UI.Sources.Page.Edit
                    |> UI.Page.Sources
                    |> GoToPage
            , active = False
            }
        , Item
            { icon = Icons.sync
            , label = "Process source"
            , msg = ProcessSources [ source ]
            , active = False
            }
        , Item
            { icon = Icons.delete
            , label = "Remove source"
            , msg = RemoveSourceFromCollection { sourceId = source.id }
            , active = False
            }
        ]
