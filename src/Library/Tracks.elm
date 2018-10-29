module Tracks exposing (Collection, Favourite, IdentifiedTrack, Identifiers, Tags, Track, emptyCollection, emptyIdentifiedTrack, emptyTags, emptyTrack, missingId)

-- 🌳


type alias Track =
    { id : String
    , path : String
    , sourceId : String
    , tags : Tags
    }



-- PIECES


type alias Tags =
    { disc : Int
    , nr : Int

    -- Main
    , album : String
    , artist : String
    , title : String

    -- Extra
    , genre : Maybe String
    , picture : Maybe String
    , year : Maybe Int
    }



-- DERIVATIVES & SUPPLEMENTS


type alias Favourite =
    { artist : String
    , title : String
    }


type alias IdentifiedTrack =
    ( Identifiers, Track )


type alias Identifiers =
    { isFavourite : Bool
    , isMissing : Bool
    , isNowPlaying : Bool
    , isSelected : Bool

    --
    , indexInList : Int
    , indexInPlaylist : Maybe Int
    }



-- COLLECTIONS


type alias Collection =
    { untouched : List Track

    -- `Track`s with `Identifiers`
    , identified : List IdentifiedTrack

    -- Sorted and filtered by playlist (if not auto-generated)
    , arranged : List IdentifiedTrack

    -- Filtered by search results, favourites, etc.
    , harvested : List IdentifiedTrack
    }



-- ⚡️


emptyTrack : Track
emptyTrack =
    { id = ""
    , path = ""
    , sourceId = ""
    , tags = emptyTags
    }


emptyTags : Tags
emptyTags =
    { disc = 1
    , nr = 0
    , album = "Empty"
    , artist = "Empty"
    , title = "Empty"
    , genre = Nothing
    , picture = Nothing
    , year = Nothing
    }


emptyIdentifiedTrack : IdentifiedTrack
emptyIdentifiedTrack =
    ( { isFavourite = False
      , isMissing = False
      , isNowPlaying = False
      , isSelected = False
      , indexInList = 0
      , indexInPlaylist = Nothing
      }
    , emptyTrack
    )


emptyCollection : Collection
emptyCollection =
    { untouched = []
    , identified = []
    , arranged = []
    , harvested = []
    }


missingId : String
missingId =
    "<missing>"
