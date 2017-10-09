module Authentication.Types exposing (..)

import Json.Encode


-- Settings Types

import Sources.Types as Sources
import Tracks.Types as Tracks


-- Messages


type Msg
    = Incoming OutgoingMsg IncomingResult
    | PerformSignIn Method
    | PerformSignOut



-- Model


type alias Model =
    { method : Maybe Method
    , signedIn : Bool
    }



-- Talking to the outside world


type OutgoingMsg
    = MethodGet
    | MethodSet
    | MethodUnset
      --
    | Construct
    | Deconstruct
    | IsSignedIn
    | IsSigningIn
    | HandleSignInProcess
    | SignIn
    | SignOut
    | GetData
    | StoreData


type alias OutgoingEvent =
    { tag : OutgoingMsg, data : Maybe Json.Encode.Value }


type alias OutsideEvent =
    { tag : String, data : Json.Encode.Value, error : Maybe String }


type alias IncomingResult =
    Result String Json.Encode.Value



-- Other


type Method
    = Blockstack
    | Local


type alias UserData =
    { favourites : Maybe (List Tracks.Favourite)
    , settings : Maybe Json.Encode.Value
    , sources : Maybe (List Sources.Source)
    , tracks : Maybe (List Tracks.Track)
    }
