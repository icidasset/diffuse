module Authentication.Transformers exposing (..)

import Authentication.Types exposing (..)
import String.Extra


-- Methods


stringToMethod : String -> Method
stringToMethod str =
    case str of
        "BLOCKSTACK" ->
            Blockstack

        "LOCAL" ->
            Local

        _ ->
            Debug.crash "Invalid authentication method"


methodToString : Method -> String
methodToString method =
    case method of
        Blockstack ->
            "BLOCKSTACK"

        Local ->
            "LOCAL"



-- Outgoing messages


stringToOutgoingMessage : String -> OutgoingMsg
stringToOutgoingMessage str =
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
            Debug.crash "Invalid OutgoingMsg"


outgoingMessageToString : OutgoingMsg -> String
outgoingMessageToString msg =
    msg
        |> toString
        |> String.Extra.underscored
        |> String.toUpper
