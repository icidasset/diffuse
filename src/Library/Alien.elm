module Alien exposing (Event, Tag(..), broadcast, report, tagFromString, tagToString, trigger)

{-| 👽 Aliens.

This involves the incoming and outgoing data.
Including the communication between the different Elm apps/workers.

-}

import Json.Encode



-- 🌳


type alias Event =
    { tag : String, data : Json.Encode.Value, error : Maybe String }


type Tag
    = AuthAnonymous
    | AuthMethod
      -- from UI
    | SignIn
    | SignOut
      -- to UI
    | HideLoadingScreen
    | LoadUserData



-- ⚡️


broadcast : Tag -> Json.Encode.Value -> Event
broadcast tag data =
    { tag = tagToString tag
    , data = data
    , error = Nothing
    }


report : Tag -> String -> Event
report tag error =
    { tag = tagToString tag
    , data = Json.Encode.null
    , error = Just error
    }


trigger : Tag -> Event
trigger tag =
    { tag = tagToString tag
    , data = Json.Encode.null
    , error = Nothing
    }


tagToString : Tag -> String
tagToString tag =
    case tag of
        AuthAnonymous ->
            "AUTH_ANONYMOUS"

        AuthMethod ->
            "AUTH_METHOD"

        -----------------------------------------
        -- From UI
        -----------------------------------------
        SignIn ->
            "SIGN_IN"

        SignOut ->
            "SIGN_OUT"

        -----------------------------------------
        -- To UI
        -----------------------------------------
        HideLoadingScreen ->
            "HIDE_LOADING_SCREEN"

        LoadUserData ->
            "LOAD_USER_DATA"


tagFromString : String -> Maybe Tag
tagFromString string =
    case string of
        "AUTH_ANONYMOUS" ->
            Just AuthAnonymous

        "AUTH_METHOD" ->
            Just AuthMethod

        -----------------------------------------
        -- From UI
        -----------------------------------------
        "SIGN_IN" ->
            Just SignIn

        "SIGN_OUT" ->
            Just SignOut

        -----------------------------------------
        -- UI
        -----------------------------------------
        "HIDE_LOADING_SCREEN" ->
            Just HideLoadingScreen

        "LOAD_USER_DATA" ->
            Just LoadUserData

        _ ->
            Nothing
