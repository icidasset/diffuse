module Authentication.Method exposing (..)

import Authentication.Types exposing (..)
import LocalStorage
import Native.Authentication.Blockstack
import Native.Authentication.Local
import Task exposing (Task)


-- Constants


storageKey : String
storageKey =
    "authenticationMethod"


type alias Error =
    String



-- Actions


construct : Method -> Task () ()
construct method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.construct

        Local ->
            Native.Authentication.Local.construct


deconstruct : Method -> Task () ()
deconstruct method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.deconstruct

        Local ->
            Native.Authentication.Local.deconstruct


isSignedIn : Method -> Task Never Bool
isSignedIn method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.isSignedIn

        Local ->
            Native.Authentication.Local.isSignedIn


isSigningIn : Method -> Task Never Bool
isSigningIn method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.isSigningIn

        Local ->
            Native.Authentication.Local.isSigningIn


handleSignInProcess : Method -> Task Never SignInProcessConsequence
handleSignInProcess method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.handleSignInProcess

        Local ->
            Native.Authentication.Local.handleSignInProcess


signIn : Method -> Task Never SignInConsequence
signIn method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.signIn

        Local ->
            Native.Authentication.Local.signIn


signOut : Method -> Task Never ()
signOut method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.signOut

        Local ->
            Native.Authentication.Local.signOut


getData : Method -> Task Error (Maybe String)
getData method =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.getData

        Local ->
            Native.Authentication.Local.getData


storeData : Method -> String -> Task Error ()
storeData method json =
    case method of
        Blockstack ->
            Native.Authentication.Blockstack.storeData json

        Local ->
            Native.Authentication.Local.storeData json



-- Local storage


get : Task LocalStorage.Error (Maybe Method)
get =
    storageKey
        |> LocalStorage.get
        |> Task.map (Maybe.map stringToType)


set : Method -> Task LocalStorage.Error ()
set method =
    LocalStorage.set
        storageKey
        (typeToString method)


remove : Task LocalStorage.Error ()
remove =
    LocalStorage.remove
        storageKey



-- Union types & stuff


stringToType : String -> Method
stringToType str =
    case String.toUpper str of
        "BLOCKSTACK" ->
            Blockstack

        "LOCAL" ->
            Local

        _ ->
            Debug.crash "Invalid authentication method"


typeToString : Method -> String
typeToString method =
    case method of
        Blockstack ->
            "BLOCKSTACK"

        Local ->
            "LOCAL"
