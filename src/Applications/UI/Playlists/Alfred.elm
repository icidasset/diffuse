module UI.Playlists.Alfred exposing (create, select)

import Alfred exposing (..)
import List.Extra as List
import Material.Icons as Icons
import Playlists exposing (..)
import Tracks exposing (IdentifiedTrack)
import UI.Types as UI



-- CREATE


create : List IdentifiedTrack -> List Playlist -> Alfred UI.Msg
create tracks playlists =
    let
        playlistNames =
            playlists
                |> List.map .name
                |> List.sortBy String.toLower

        index =
            makeIndex playlistNames
    in
    { action = createAction tracks
    , focus = 0
    , index = index
    , message =
        if List.length tracks == 1 then
            "Choose or create a playlist to add this track to."

        else
            "Choose or create a playlist to add these tracks to."
    , operation = QueryOrMutation
    , results = index
    , searchTerm = Nothing
    }


createAction : List IdentifiedTrack -> Alfred.Action UI.Msg
createAction tracks maybe =
    let
        playlistTracks =
            Tracks.toPlaylistTracks tracks
    in
    case maybe.result of
        Just result ->
            -- Add to playlist
            --
            [ UI.AddTracksToPlaylist
                { playlistName = result.value
                , tracks = playlistTracks
                }
            ]

        Nothing ->
            -- Create playlist,
            -- if given a search term.
            --
            case maybe.searchTerm of
                Just searchTerm ->
                    [ UI.AddTracksToPlaylist
                        { playlistName = searchTerm
                        , tracks = playlistTracks
                        }
                    ]

                Nothing ->
                    []



-- SELECT


select : List Playlist -> Alfred UI.Msg
select playlists =
    let
        playlistNames =
            playlists
                |> List.map .name
                |> List.sortBy String.toLower

        index =
            makeIndex playlistNames
    in
    { action = selectAction playlists
    , focus = 0
    , index = index
    , message = "Select a playlist to play tracks from."
    , operation = Query
    , results = index
    , searchTerm = Nothing
    }


selectAction : List Playlist -> Alfred.Action UI.Msg
selectAction playlists { result } =
    case Maybe.andThen (\r -> List.find (.name >> (==) r.value) playlists) result of
        Just playlist ->
            [ UI.SelectPlaylist playlist ]

        Nothing ->
            []



-- ㊙️


makeIndex playlistNames =
    List.map
        (\name ->
            { icon = Just Icons.queue_music
            , title = name
            , value = name
            }
        )
        playlistNames
