module Tracks.Utils exposing (..)

import Tracks.Types exposing (..)


emptyTags : Tags
emptyTags =
    { album = Nothing
    , artist = Nothing
    , genre = Nothing
    , title = Nothing
    , track = Nothing
    , year = Nothing
    }


emptyTrack : Track
emptyTrack =
    { path = ""
    , sourceId = ""
    , tags = emptyTags
    }


makeTrack : String -> ( String, Tags ) -> Track
makeTrack sourceId ( path, tags ) =
    { path = path
    , sourceId = sourceId
    , tags = tags
    }
