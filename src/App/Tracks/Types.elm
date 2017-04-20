module Tracks.Types exposing (..)

import Base64
import Regex exposing (HowMany(..), regex)


-- `Track` record


type alias Tags =
    { nr : Int

    -- Main
    , album : String
    , artist : String
    , title : String

    -- Extra
    , genre : Maybe String
    , year : Maybe Int
    }


type alias TagUrls =
    { getUrl : String
    , headUrl : String
    }


type alias Track =
    { id : TrackId
    , path : String
    , sourceId : SourceId
    , tags : Tags
    }


type alias SourceId =
    String


type alias TrackId =
    String



-- Sorting


type SortBy
    = Artist
    | Album
    | Title


type SortDirection
    = Asc
    | Desc



-- Other


type Msg
    = Add (List Track)
    | Remove SourceId
    | RemoveByPath SourceId (List String)
    | Recalibrate
    | SortBy SortBy
      -- Search
    | ReceiveSearchResults (List SourceId)
    | Search
    | SetSearchTerm String
      -- UI
    | ScrollThroughTable ScrollPos


type alias Model =
    { collection : List Track
    , resultant : List Track -- Use this for the UI
    , searchResults : List Track
    , searchTerm : Maybe String
    , sortBy : SortBy
    , sortDirection : SortDirection
    }


type alias ScrollPos =
    { scrolledHeight : Int
    , contentHeight : Int
    , containerHeight : Int
    }



-- 🌱


emptyTags : Tags
emptyTags =
    { nr = 0
    , album = "Empty"
    , artist = "Empty"
    , title = "Empty"
    , genre = Nothing
    , year = Nothing
    }


emptyTrack : Track
emptyTrack =
    { id = ""
    , path = ""
    , sourceId = ""
    , tags = emptyTags
    }


makeTrack : String -> ( String, Tags ) -> Track
makeTrack sourceId ( path, tags ) =
    { id =
        let
            id =
                sourceId ++ "//" ++ path
        in
            id
                |> Base64.encode
                |> Result.withDefault (id)
                |> Regex.replace All (regex "=+$") (\_ -> "")
    , path = path
    , sourceId = sourceId
    , tags = tags
    }
