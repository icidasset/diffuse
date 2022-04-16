module UI.Playlists.Alfred exposing (create, select)

import Alfred exposing (..)
import Json.Decode exposing (string)
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
    Alfred.create
        { action = createAction tracks
        , index = index
        , message =
            if List.length tracks == 1 then
                "Choose or create a playlist to add this track to."

            else
                "Choose or create a playlist to add these tracks to."
        , operation = QueryOrMutation
        }


createAction : List IdentifiedTrack -> Alfred.Action UI.Msg
createAction tracks ctx =
    let
        playlistTracks =
            Tracks.toPlaylistTracks tracks
    in
    case ctx.result of
        Just result ->
            -- Add to playlist
            --
            case Alfred.stringValue result.value of
                Just playlistName ->
                    [ UI.AddTracksToPlaylist
                        { playlistName = playlistName
                        , tracks = playlistTracks
                        }
                    ]

                Nothing ->
                    []

        Nothing ->
            -- Create playlist,
            -- if given a search term.
            --
            case ctx.searchTerm of
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
    Alfred.create
        { action = selectAction playlists
        , index = index
        , message = "Select a playlist to play tracks from."
        , operation = Query
        }


selectAction : List Playlist -> Alfred.Action UI.Msg
selectAction playlists { result } =
    case Maybe.andThen (\r -> List.find (.name >> Just >> (==) (stringValue r.value)) playlists) result of
        Just playlist ->
            [ UI.SelectPlaylist playlist ]

        Nothing ->
            []



-- ㊙️


makeIndex playlistNames =
    playlistNames
        |> List.map
            (\name ->
                { icon = Just (Icons.queue_music 16)
                , title = name
                , value = Alfred.StringValue name
                }
            )
        |> (\items ->
                { name = Nothing, items = items }
           )
        |> List.singleton
