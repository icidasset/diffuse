module Playlists exposing (..)

-- 🌳


type alias Playlist =
    { autoGenerated : Bool
    , name : String
    , public : Bool
    , tracks : List PlaylistTrack
    }


type alias PlaylistTrack =
    { album : Maybe String
    , artist : Maybe String
    , title : String
    }


type alias IdentifiedPlaylistTrack =
    ( Identifiers, PlaylistTrack )


type alias Identifiers =
    { index : Int }
