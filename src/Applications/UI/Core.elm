module UI.Core exposing (Flags, Model, Msg(..), Switch(..))

import Alien
import Browser
import Browser.Navigation as Nav
import File exposing (File)
import Json.Encode as Json
import Time
import UI.Authentication
import UI.Backdrop
import UI.Page exposing (Page)
import UI.Sources
import UI.Tracks
import Url exposing (Url)



-- ⛩


type alias Flags =
    {}



-- 🌳


type alias Model =
    { isAuthenticated : Bool
    , isLoading : Bool
    , navKey : Nav.Key
    , page : Page
    , url : Url

    -----------------------------------------
    -- Children
    -----------------------------------------
    , authentication : UI.Authentication.Model
    , backdrop : UI.Backdrop.Model
    , sources : UI.Sources.Model
    , tracks : UI.Tracks.Model
    }



-- 📣


type Msg
    = Bypass
    | LoadEnclosedUserData Json.Value
    | LoadHypaethralUserData Json.Value
    | SetCurrentTime Time.Posix
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Brain
      -----------------------------------------
    | ProcessSources
    | SaveEnclosedUserData
    | SaveFavourites
    | SaveSources
    | SaveTracks
    | SignOut
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AuthenticationMsg UI.Authentication.Msg
    | BackdropMsg UI.Backdrop.Msg
    | SourcesMsg UI.Sources.Msg
    | TracksMsg UI.Tracks.Msg
      -----------------------------------------
      -- Import / Export
      -----------------------------------------
    | Export
    | Import File
    | ImportJson String
    | RequestImport
      -----------------------------------------
      -- URL
      -----------------------------------------
    | ChangeUrlUsingPage Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


type Switch
    = On
    | Off
