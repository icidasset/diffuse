module UI.Tracks.Core exposing (Model, Msg(..), Scene(..))

import InfiniteList
import Json.Encode as Json
import Tracks exposing (..)
import UI.Reply exposing (Reply)



-- 🌳


type alias Model =
    { collection : Collection
    , enabledSourceIds : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , hideDuplicates : Bool
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
    | Reply (List Reply)
    | ScrollToNowPlaying
    | SetEnabledSourceIds (List String)
    | SetNowPlaying (Maybe IdentifiedTrack)
    | SortBy SortBy
    | ToggleHideDuplicates
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
