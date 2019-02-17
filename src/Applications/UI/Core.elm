module UI.Core exposing (Flags, Model, Msg(..), Switch(..))

import Alien
import Browser
import Browser.Navigation as Nav
import File exposing (File)
import Json.Encode as Json
import Time
import UI.Authentication as Authentication
import UI.Backdrop as Backdrop
import UI.Page exposing (Page)
import UI.Sources as Sources
import UI.Tracks.Core as Tracks
import Url exposing (Url)



-- ⛩


type alias Flags =
    { viewport : Viewport }



-- 🌳


type alias Model =
    { isAuthenticated : Bool
    , isLoading : Bool
    , navKey : Nav.Key
    , page : Page
    , url : Url
    , viewport : Viewport

    -----------------------------------------
    -- Children
    -----------------------------------------
    , authentication : Authentication.Model
    , backdrop : Backdrop.Model
    , sources : Sources.Model
    , tracks : Tracks.Model
    }


type alias Viewport =
    { height : Float
    , width : Float
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
    | AuthenticationMsg Authentication.Msg
    | BackdropMsg Backdrop.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
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
