module UI.Sources.ContextMenu exposing (sourceMenu)

import Conditional exposing (ifThenElse)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Round as Icons
import Sources exposing (Source)
import UI.Page
import UI.Sources.Page
import UI.Sources.Types as Sources
import UI.Types exposing (Msg(..))



-- 🔱


sourceMenu : Source -> Coordinates -> ContextMenu Msg
sourceMenu source =
    ContextMenu
        [ Item
            { icon = ifThenElse source.directoryPlaylists Icons.folder Icons.folder_open
            , label = ifThenElse source.directoryPlaylists "Disable Directory Playlists" "Enable Directory Playlists"
            , msg =
                { sourceId = source.id }
                    |> Sources.ToggleDirectoryPlaylists
                    |> SourcesMsg

            --
            , active = False
            }

        --
        , Item
            { icon = Icons.edit
            , label = "Edit source"
            , msg =
                source.id
                    |> UI.Sources.Page.Edit
                    |> UI.Page.Sources
                    |> ChangeUrlUsingPage

            --
            , active = False
            }

        --
        , Item
            { icon = Icons.sync
            , label = "Process source"
            , msg =
                [ source ]
                    |> Sources.ProcessSpecific
                    |> SourcesMsg

            --
            , active = False
            }

        --
        , Item
            { icon = Icons.delete
            , label = "Remove source"
            , msg =
                { sourceId = source.id }
                    |> Sources.RemoveFromCollection
                    |> SourcesMsg
            , active = False
            }

        --
        , Item
            { icon = Icons.font_download
            , label = "Rename source"
            , msg =
                source.id
                    |> UI.Sources.Page.Rename
                    |> UI.Page.Sources
                    |> ChangeUrlUsingPage

            --
            , active = False
            }
        ]
