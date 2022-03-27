module UI.Commands.Alfred exposing (..)

import Alfred exposing (..)
import List.Extra as List
import Playlists exposing (..)
import Tracks exposing (IdentifiedTrack)
import UI.Types as UI


fred =
    { action = UI.Bypass
    , focus = 0
    , index = []
    , message = "Run a command."
    , operation = Query
    , results = []
    , searchTerm = Nothing
    }



-- SELECT
--
-- select : List Playlist -> Alfred UI.Msg
-- select playlists =
--     let
--         playlistNames =
--             playlists
--                 |> List.map .name
--                 |> List.sortBy String.toLower
--     in
--     { action = selectAction playlists
--     , focus = 0
--     , index = playlistNames
--     , message = "Run a command."
--     , operation = Query
--     , results = playlistNames
--     , searchTerm = Nothing
--     }
--
-- selectAction : List Playlist -> { result : Maybe String, searchTerm : Maybe String } -> List UI.Msg
-- selectAction playlists { result } =
--     case Maybe.andThen (\r -> List.find (.name >> (==) r) playlists) result of
--         Just playlist ->
--             [ UI.SelectPlaylist playlist ]
--         Nothing ->
--             []
