module UI.Playlists.ContextMenu exposing (listMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons as Icons
import Playlists exposing (Playlist)
import Playlists.Matching
import Tracks exposing (IdentifiedTrack)
import UI.Page
import UI.Playlists.Page
import UI.Reply exposing (Reply(..))
import Url



-- 🔱


listMenu : Playlist -> List IdentifiedTrack -> Maybe String -> Coordinates -> ContextMenu Reply
listMenu playlist allTracks confirmation coordinates =
    let
        ( identifiedTracksFromPlaylist, _ ) =
            Playlists.Matching.match playlist allTracks

        tracksFromPlaylist =
            identifiedTracksFromPlaylist
                |> List.sortBy (Tuple.first >> .indexInPlaylist >> Maybe.withDefault 0)
                |> List.map Tuple.second

        playlistId =
            "Playlist - " ++ playlist.name

        menuReply =
            ShowPlaylistListMenu coordinates playlist

        askForConfirmation =
            confirmation == Just playlistId
    in
    ContextMenu
        [ Item
            { icon = Icons.archive
            , label = "Download as zip file"
            , msg = DownloadTracks playlist.name tracksFromPlaylist
            , active = False
            }
        , Item
            { icon = Icons.font_download
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
            , label =
                if askForConfirmation then
                    "Are you sure?"

                else
                    "Remove playlist"
            , msg =
                if askForConfirmation then
                    RemovePlaylistFromCollection { playlistName = playlist.name }

                else
                    ContextMenuConfirmation playlistId menuReply
            , active =
                askForConfirmation
            }
        , Item
            { icon = Icons.offline_bolt
            , label = "Store in cache"
            , msg = StoreTracksInCache tracksFromPlaylist
            , active = False
            }
        ]
        coordinates
