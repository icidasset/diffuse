module UI.Playlists.ContextMenu exposing (listMenu)

import Conditional exposing (ifThenElse)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.File as Icons
import Material.Icons.Image as Icons
import Playlists exposing (Playlist)
import Playlists.Matching
import Tracks exposing (IdentifiedTrack)
import UI.Page
import UI.Playlists as Playlists
import UI.Playlists.Page
import UI.Reply exposing (Reply(..))
import Url



-- 🔱


listMenu : Playlist -> List IdentifiedTrack -> Coordinates -> ContextMenu Reply
listMenu playlist tracks =
    ContextMenu
        [ Item
            { icon = Icons.edit
            , label = "Rename playlist"
            , msg =
                playlist.name
                    |> Url.percentEncode
                    |> UI.Playlists.Page.Edit
                    |> UI.Page.Playlists
                    |> GoToPage
            , active = False
            }
        , Item
            { icon = Icons.delete
            , label = "Remove playlist"
            , msg = RemovePlaylistFromCollection { playlistName = playlist.name }
            , active = False
            }
        , Item
            { icon = Icons.offline_bolt
            , label = "Store in cache"
            , msg =
                tracks
                    |> Playlists.Matching.match playlist
                    |> Tuple.first
                    |> List.map Tuple.second
                    |> StoreTracksInCache
            , active = False
            }
        ]
