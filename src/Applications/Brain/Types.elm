module Brain.Types exposing (..)

import Brain.Sources.Processing.Types as Processing
import Brain.User.Types as User
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Json.Decode as Json
import List.Zipper exposing (Zipper)
import Management
import Sources.Processing as Processing
import Time
import User.Layer as User exposing (HypaethralBit(..))



-- 🧠


type alias Flags =
    { initialUrl : String }



-- 🌳


type alias Model =
    { authMethod : Maybe User.Method
    , currentTime : Time.Posix
    , hypaethralDebouncer : Debouncer HypaethralBit (List HypaethralBit)
    , hypaethralRetrieval : Maybe (Zipper ( HypaethralBit, Json.Value ))
    , hypaethralStorage : List HypaethralBit
    , hypaethralUserData : User.HypaethralData
    , legacyMode : Bool
    , origin : String
    , performingSignIn : Bool
    , processingStatus : Processing.Status
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
    | RemoveTracksBySourceId Json.Value
    | RemoveTracksFromCache Json.Value
    | Search Json.Value
    | StoreTracksInCache Json.Value
    | UpdateSearchIndex Json.Value
      -----------------------------------------
      -- 🦉 Nested
      -----------------------------------------
    | ProcessingMsg Processing.Msg
    | UserMsg User.Msg
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | RedirectToBlockstackSignIn
    | SetCurrentTime Time.Posix
    | ToCache Json.Value


type alias Organizer model =
    Management.Manager Msg model


type alias Manager =
    Organizer Model
