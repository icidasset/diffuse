module Tracks exposing (Collection, CollectionDependencies, Favourite, Grouping(..), IdentifiedTrack, Identifiers, Parcel, SortBy(..), SortDirection(..), Tags, Track, emptyCollection, emptyIdentifiedTrack, emptyIdentifiers, emptyTags, emptyTrack, isNowPlaying, makeTrack, missingId, removeFromPlaylist, toPlaylistTracks)

import Base64
import List.Extra as List
import Playlists exposing (Playlist, PlaylistTrack)
import String.Ext as String
import Time
import Time.Ext as Time



-- 🌳


type alias Track =
    { id : String
    , insertedAt : Time.Posix
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
    , group : Maybe { name : String, firstInGroup : Bool }
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


type alias CollectionDependencies =
    { enabledSourceIds : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , grouping : Maybe Grouping
    , hideDuplicates : Bool
    , nowPlaying : Maybe IdentifiedTrack
    , selectedPlaylist : Maybe Playlist
    , searchResults : Maybe (List String)
    , sortBy : SortBy
    , sortDirection : SortDirection
    }


type alias Parcel =
    ( CollectionDependencies, Collection )



-- GROUPING & SORTING


type Grouping
    = AddedOnGroups
    | Directory
    | TrackYearGroups


type SortBy
    = Artist
    | Album
    | PlaylistIndex
    | Title


type SortDirection
    = Asc
    | Desc



-- 🔱


emptyTrack : Track
emptyTrack =
    { id = ""
    , insertedAt = Time.default
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
    ( emptyIdentifiers
    , emptyTrack
    )


emptyIdentifiers : Identifiers
emptyIdentifiers =
    { isFavourite = False
    , isMissing = False
    , isNowPlaying = False
    , isSelected = False

    --
    , group = Nothing
    , indexInList = 0
    , indexInPlaylist = Nothing
    }


emptyCollection : Collection
emptyCollection =
    { untouched = []
    , identified = []
    , arranged = []
    , harvested = []
    }


isNowPlaying : IdentifiedTrack -> IdentifiedTrack -> Bool
isNowPlaying ( a, b ) ( x, y ) =
    a.indexInPlaylist == x.indexInPlaylist && b.id == y.id


makeTrack : String -> ( String, Tags ) -> Track
makeTrack sourceId ( path, tags ) =
    { id =
        (sourceId ++ "//" ++ path)
            |> Base64.encode
            |> String.chopEnd "="
    , insertedAt = Time.default
    , path = path
    , sourceId = sourceId
    , tags = tags
    }


removeFromPlaylist : List IdentifiedTrack -> Playlist -> Playlist
removeFromPlaylist tracks playlist =
    playlist.tracks
        |> List.indexedFoldr
            (\idx t ( acc, remaining ) ->
                case List.partition ((==) (Just idx)) remaining of
                    ( match :: _, rem ) ->
                        ( acc, rem )

                    ( _, rem ) ->
                        ( t :: acc, rem )
            )
            ( []
            , List.map (Tuple.first >> .indexInPlaylist) tracks
            )
        |> (\( t, _ ) -> { playlist | tracks = t })


missingId : String
missingId =
    "<missing>"


toPlaylistTracks : List IdentifiedTrack -> List PlaylistTrack
toPlaylistTracks =
    List.map
        (\( i, t ) ->
            { album = t.tags.album
            , artist = t.tags.artist
            , title = t.tags.title
            }
        )
