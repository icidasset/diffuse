module Playlists.Types exposing (..)

-- Playlist


type alias Playlist =
    { autoGenerated : Bool
    , name : String
    , tracks : List PlaylistTrack
    }


type alias PlaylistTrack =
    { album : String
    , artist : String
    , title : String
    }



-- Messages


type Msg
    = SetCollection (List Playlist)
      -- Creation
    | Create
    | SetNewPlaylistName String
      -- Removal
    | Remove String



-- Model


type alias Model =
    { collection : List Playlist
    , newPlaylist : Playlist
    }



-- Pages


type Page
    = Index
    | New
