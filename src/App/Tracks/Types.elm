module Tracks.Types exposing (..)

import Base64
import Json.Encode as Json
import Regex exposing (HowMany(..), regex)


-- `Tags` record


type alias Tags =
    { disc : Int
    , nr : Int

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



-- `Track` record


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



-- `IdentifiedTrack` record


type alias IdentifiedTrack =
    ( Identifiers, Track )


type alias Identifiers =
    { isFavourite : Bool
    , isMissing : Bool
    , isNowPlaying : Bool
    }



-- `Favourite` record


type alias Favourite =
    { artist : String
    , title : String
    }



-- Sorting


type SortBy
    = Artist
    | Album
    | Title


type SortDirection
    = Asc
    | Desc



-- Collections


type alias Collection =
    { untouched : List Track

    -- `Track`s with `Identifiers`
    , identified : List IdentifiedTrack

    -- Filtered by search results and favourites
    , harvested : List IdentifiedTrack

    -- Partial rendering of the harvested collection in the UI
    , exposed : List IdentifiedTrack
    }


type alias Parcel =
    ( Model, Collection )



-- Messages


type Msg
    = Recalibrate
    | Reharvest
    | SortBy SortBy
      -- Collection Pt. 1
    | InitialCollection (List Json.Value)
      -- Collection Pt. 2
    | Add (List Track)
    | Remove SourceId
    | RemoveByPath SourceId (List String)
      -- Search
    | ReceiveSearchResults (List SourceId)
    | Search (Maybe String)
    | SetSearchTerm String
      -- Favourites
    | ToggleFavourite String
    | ToggleFavouritesOnly
      -- UI
    | ScrollThroughTable ScrollPos
    | ScrollToActiveTrack Track
    | IdentifyActiveTrack (Maybe Track)



-- Model


type alias Model =
    InternalModel Settings


type alias InternalModel extension =
    { extension
        | collection : Collection
        , exposedStep : Int
        , favourites : List Favourite
        , searchResults : Maybe (List TrackId)
        , sortBy : SortBy
        , sortDirection : SortDirection
    }


type alias Settings =
    { favouritesOnly : Bool -- Whether or not to only show favourites in the UI
    , searchTerm : Maybe String
    }



-- Other


type alias ScrollPos =
    { scrolledHeight : Int
    , contentHeight : Int
    , containerHeight : Int
    }



-- 🌱


emptyTags : Tags
emptyTags =
    { disc = 1
    , nr = 0
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


emptyCollection : Collection
emptyCollection =
    { untouched = []
    , identified = []
    , harvested = []
    , exposed = []
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
