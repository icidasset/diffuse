module Authentication.Translations exposing (..)

import Authentication.Types exposing (..)


-- Methods


stringToMethod : String -> Method
stringToMethod str =
    case str of
        "BLOCKSTACK" ->
            Blockstack

        "LOCAL" ->
            Local

        "REMOTE_STORAGE" ->
            RemoteStorage

        _ ->
            Debug.crash "Invalid authentication method"



-- Alien messages


stringToAlienMessage : String -> AlienMsg
stringToAlienMessage str =
    case str of
        "METHOD_GET" ->
            MethodGet

        "METHOD_SET" ->
            MethodSet

        "METHOD_UNSET" ->
            MethodUnset

        "CONSTRUCT" ->
            Construct

        "DECONSTRUCT" ->
            Deconstruct

        "IS_SIGNED_IN" ->
            IsSignedIn

        "IS_SIGNING_IN" ->
            IsSigningIn

        "HANDLE_SIGN_IN_PROCESS" ->
            HandleSignInProcess

        "SIGN_IN" ->
            SignIn

        "SIGN_OUT" ->
            SignOut

        "GET_DATA" ->
            GetData

        "STORE_DATA" ->
            StoreData

        _ ->
            Debug.crash "Invalid AlienMsg"
