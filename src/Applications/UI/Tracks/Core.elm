module UI.Tracks.Core exposing (Model, Msg(..), Scene(..))

import InfiniteList
import Json.Encode as Json
import Tracks exposing (..)



-- 🌳


type alias Model =
    { collection : Collection
    , enabledSourceIds : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , infiniteList : InfiniteList.Model
    , nowPlaying : Maybe IdentifiedTrack
    , scene : Scene
    , searchResults : Maybe (List String)
    , searchTerm : Maybe String
    , sortBy : SortBy
    , sortDirection : SortDirection
    }



-- 📣


type Msg
    = Bypass
    | InfiniteListMsg InfiniteList.Model
    | Play IdentifiedTrack
    | ScrollToNowPlaying
    | SetEnabledSourceIds (List String)
    | SetNowPlaying (Maybe IdentifiedTrack)
    | SortBy SortBy
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | Add Json.Value
    | RemoveByPaths Json.Value
    | RemoveBySourceId String
      -----------------------------------------
      -- Favourites
      -----------------------------------------
    | ToggleFavourite Int
    | ToggleFavouritesOnly
      -----------------------------------------
      -- Search
      -----------------------------------------
    | ClearSearch
    | Search
    | SetSearchResults Json.Value
    | SetSearchTerm String



-- OTHER


type Scene
    = List
