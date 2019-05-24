module UI.Playlists.ContextMenu exposing (listMenu)

import Conditional exposing (ifThenElse)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.File as Icons
import Material.Icons.Image as Icons
import Playlists exposing (Playlist)
import UI.Core exposing (Msg(..))
import UI.Page
import UI.Playlists as Playlists
import UI.Playlists.Page
import Url



-- 🔱


listMenu : Playlist -> Coordinates -> ContextMenu Msg
listMenu playlist =
    ContextMenu
        [ Item
            { icon = Icons.edit
            , label = "Rename playlist"
            , msg =
                playlist.name
                    |> Url.percentEncode
                    |> UI.Playlists.Page.Edit
                    |> UI.Page.Playlists
                    |> ChangeUrlUsingPage
            , active = False
            }
        , Item
            { icon = Icons.delete
            , label = "Remove playlist"
            , msg = PlaylistsMsg (Playlists.RemoveFromCollection { playlistName = playlist.name })
            , active = False
            }
        ]
