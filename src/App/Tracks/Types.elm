module Tracks.Types exposing (..)

-- `Track` record


type alias Tags =
    { album : Maybe String
    , artist : Maybe String
    , genre : Maybe String
    , title : Maybe String
    , track : Maybe Int
    , year : Maybe Int
    }


type alias TagUrls =
    { getUrl : String
    , headUrl : String
    }


type alias Track =
    { path : String
    , sourceId : String
    , tags : Tags
    }



-- 🌱


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
