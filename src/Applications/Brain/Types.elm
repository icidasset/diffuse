module Brain.Types exposing (..)

import Brain.Sources.Processing.Types as Processing
import Brain.User.Types as User
import Debouncer.Basic exposing (Debouncer)
import Json.Decode as Json
import List.Zipper exposing (Zipper)
import Management
import Sources.Processing as Processing
import Time
import User.Layer as User exposing (HypaethralBaggage, HypaethralBit)



-- 🧠


type alias Flags =
    { initialUrl : String }



-- 🌳


type alias Model =
    { currentTime : Time.Posix
    , hypaethralDebouncer : Debouncer HypaethralBit (List HypaethralBit)
    , hypaethralRetrieval : Maybe (Zipper ( HypaethralBit, Json.Value, HypaethralBaggage ))
    , hypaethralStorage : List { bit : HypaethralBit, saving : Bool }
    , hypaethralUserData : User.HypaethralData
    , origin : String
    , processingStatus : Processing.Status
    , userSyncMethod : Maybe User.Method
    }



-- 📣


type Msg
    = Bypass
    | Cmd (Cmd Msg)
      -----------------------------------------
      -- Tracks
      -----------------------------------------
    | DownloadTracks Json.Value
    | GotSearchResults (List String)
    | MakeArtworkTrackUrls Json.Value
    | RemoveTracksBySourceId Json.Value
    | RemoveTracksFromCache Json.Value
    | ReplaceTrackTags Processing.ContextForTagsSync
    | Search Json.Value
    | StoreTracksInCache Json.Value
    | SyncTrackTags Json.Value
    | UpdateSearchIndex Json.Value
      -----------------------------------------
      -- 🦉 Nested
      -----------------------------------------
    | ProcessingMsg Processing.Msg
    | UserMsg User.Msg
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | RefreshedAccessToken Json.Value
    | SetCurrentTime Time.Posix
    | ToCache Json.Value


type alias Organizer model =
    Management.Manager Msg model


type alias Manager =
    Organizer Model
