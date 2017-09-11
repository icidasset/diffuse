module Authentication.Types exposing (..)

import Json.Encode
import LocalStorage


-- Settings Types

import Equalizer.Types as Equalizer
import Queue.Types as Queue
import Settings.Types as Settings
import Sources.Types as Sources
import Tracks.Types as Tracks


-- Messages


type Msg
    = DidGetData (Result String (Maybe String))
    | DidGetMethod (Result LocalStorage.Error (Maybe Method))
    | DidGetIsSignedIn Bool
    | DidGetIsSigningIn Bool
    | DidConstruct (Result () ())
    | DidSignIn SignInConsequence
    | DidHandleSignInProcess SignInProcessConsequence
      --
    | SignIn Method
    | SignOut



-- Model


type alias Model =
    { method : Maybe Method
    , signedIn : Bool
    }



-- Other


type Method
    = Blockstack
    | Local


type SignInConsequence
    = None
    | Redirect


type SignInProcessConsequence
    = KeepUrl
    | ModifyUrl


type alias UserData =
    { favourites : Maybe (List Tracks.Favourite)
    , settings : Maybe Json.Encode.Value
    , sources : Maybe (List Sources.Source)
    , tracks : Maybe (List Tracks.Track)
    }
