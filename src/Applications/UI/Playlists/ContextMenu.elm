module UI.Playlists.ContextMenu exposing (listMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Html.Events.Extra.Mouse
import Material.Icons as Icons
import Playlists exposing (Playlist)
import Playlists.Matching
import Tracks exposing (IdentifiedTrack)
import UI.Page
import UI.Playlists.Page
import UI.Tracks.Types as Tracks
import UI.Types exposing (Msg(..))
import Url



-- 🔱


listMenu : Playlist -> List IdentifiedTrack -> Maybe String -> Coordinates -> ContextMenu Msg
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

        menuMsg =
            ShowPlaylistListMenu
                playlist
                { button = Html.Events.Extra.Mouse.MainButton
                , clientPos = Coordinates.toTuple coordinates
                , keys = { alt = False, ctrl = False, shift = False }
                , offsetPos = ( 0, 0 )
                , pagePos = ( 0, 0 )
                , screenPos = ( 0, 0 )
                }

        askForConfirmation =
            confirmation == Just playlistId
    in
    ContextMenu
        [ Item
            { icon = Icons.archive
            , label = "Download as zip file"
            , msg =
                tracksFromPlaylist
                    |> Tracks.Download playlist.name
                    |> TracksMsg

            --
            , active = False
            }

        --
        , Item
            { icon = Icons.font_download
            , label = "Rename playlist"
            , msg =
                playlist.name
                    |> Url.percentEncode
                    |> UI.Playlists.Page.Edit
                    |> UI.Page.Playlists
                    |> ChangeUrlUsingPage

            --
            , active = False
            }

        --
        , Item
            { icon = Icons.delete
            , label =
                if askForConfirmation then
                    "Are you sure?"

                else
                    "Remove playlist"
            , msg =
                if askForConfirmation then
                    DeletePlaylist { playlistName = playlist.name }

                else
                    ContextMenuConfirmation playlistId menuMsg
            , active =
                askForConfirmation
            }

        --
        , Item
            { icon = Icons.offline_bolt
            , label = "Store in cache"
            , msg = TracksMsg (Tracks.StoreInCache tracksFromPlaylist)
            , active = False
            }
        ]
        coordinates
