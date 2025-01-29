module UI.Tracks.Types exposing (..)

import Coordinates exposing (Coordinates)
import Html.Events.Extra.Mouse as Mouse
import InfiniteList
import Json.Decode as Json
import Tracks exposing (..)



-- 📣


type Msg
    = Download { prefixTrackNumber : Bool, zipName : String } (List Track)
    | DownloadFinished
    | Harvest
    | MarkAsSelected Int { shiftKey : Bool }
    | ScrollToNowPlaying
    | SyncTags (List Track)
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
      ---------
      -- Covers
      ---------
    | GotCachedCover Json.Value
    | InsertCoverCache Json.Value
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | Add Json.Value
    | AddFavourites (List IdentifiedTrack)
    | Reload Json.Value
    | RemoveByPaths Json.Value
    | RemoveBySourceId String
    | RemoveFavourites (List IdentifiedTrack)
    | SortBy SortBy
    | ToggleFavourite Int
    | ToggleCoverSelectionReducesPool
      -----------------------------------------
      -- Groups
      -----------------------------------------
    | DisableGrouping
    | GroupBy Grouping
      -----------------------------------------
      -- Menus
      -----------------------------------------
    | ShowCoverMenu Cover Coordinates
    | ShowCoverMenuWithSmallDelay Cover Coordinates
    | ShowTracksMenu (Maybe Int) { alt : Bool } Coordinates
    | ShowTracksMenuWithSmallDelay (Maybe Int) { alt : Bool } Coordinates
    | ShowViewMenu (Maybe Grouping) Mouse.Event
      -----------------------------------------
      -- Scenes
      -----------------------------------------
    | ChangeScene Scene
    | DeselectCover
    | InfiniteListMsg InfiniteList.Model
    | SelectCover Cover
      -----------------------------------------
      -- Search
      -----------------------------------------
    | ClearSearch
    | Search
    | SetSearchResults Json.Value
    | SetSearchTerm String
