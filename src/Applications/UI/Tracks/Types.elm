module UI.Tracks.Types exposing (..)

import Coordinates exposing (Coordinates)
import Html.Events.Extra.Mouse as Mouse
import InfiniteList
import Json.Decode as Json
import Tracks exposing (..)



-- 🌳


type Scene
    = List



-- 📣


type Msg
    = Download String (List Track)
    | DownloadFinished
    | Harvest
    | MarkAsSelected Int { shiftKey : Bool }
    | ScrollToNowPlaying
    | ToggleCachedOnly
    | ToggleFavouritesOnly
    | ToggleHideDuplicates
      -----------------------------------------
      -- Cache
      -----------------------------------------
    | ClearCache
    | RemoveFromCache (List Track)
    | StoreInCache (List Track)
    | StoredInCache Json.Value (Maybe String)
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | Add Json.Value
    | RemoveByPaths Json.Value
    | RemoveBySourceId String
    | SortBy SortBy
    | ToggleFavourite Int
      -----------------------------------------
      -- Groups
      -----------------------------------------
    | DisableGrouping
    | GroupBy Grouping
      -----------------------------------------
      -- Menus
      -----------------------------------------
    | ShowTracksMenu (Maybe Int) { alt : Bool } Coordinates
    | ShowTracksMenuWithSmallDelay (Maybe Int) { alt : Bool } Coordinates
    | ShowViewMenu (Maybe Grouping) Mouse.Event
      -----------------------------------------
      -- Scenes
      -----------------------------------------
    | InfiniteListMsg InfiniteList.Model
      -----------------------------------------
      -- Search
      -----------------------------------------
    | ClearSearch
    | Search
    | SetSearchResults Json.Value
    | SetSearchTerm String
