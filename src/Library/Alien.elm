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
    | AuthEnclosedData
    | AuthMethod
      -- from UI
    | ProcessSources
    | SaveEnclosedUserData
    | SaveHypaethralUserData
    | SignIn
    | SignOut
      -- to UI
    | HideLoadingScreen
    | LoadEnclosedUserData
    | LoadHypaethralUserData
    | ReportGenericError
    | ReportSourceProcessingError



-- 🔱


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

        AuthEnclosedData ->
            "AUTH_ENCLOSED_DATA"

        -----------------------------------------
        -- From UI
        -----------------------------------------
        ProcessSources ->
            "PROCESS_SOURCES"

        SaveEnclosedUserData ->
            "SAVE_ENCLOSED_USER_DATA"

        SaveHypaethralUserData ->
            "SAVE_HYPAETHRAL_USER_DATA"

        SignIn ->
            "SIGN_IN"

        SignOut ->
            "SIGN_OUT"

        -----------------------------------------
        -- To UI
        -----------------------------------------
        HideLoadingScreen ->
            "HIDE_LOADING_SCREEN"

        LoadEnclosedUserData ->
            "LOAD_ENCLOSED_USER_DATA"

        LoadHypaethralUserData ->
            "LOAD_HYPAETHRAL_USER_DATA"

        ReportGenericError ->
            "REPORT_GENERIC_ERROR"

        ReportSourceProcessingError ->
            "REPORT_SOURCE_PROCESSING_ERROR"


tagFromString : String -> Maybe Tag
tagFromString string =
    case string of
        "AUTH_ANONYMOUS" ->
            Just AuthAnonymous

        "AUTH_METHOD" ->
            Just AuthMethod

        "AUTH_ENCLOSED_DATA" ->
            Just AuthEnclosedData

        -----------------------------------------
        -- From UI
        -----------------------------------------
        "PROCESS_SOURCES" ->
            Just ProcessSources

        "SAVE_ENCLOSED_USER_DATA" ->
            Just SaveEnclosedUserData

        "SAVE_HYPAETHRAL_USER_DATA" ->
            Just SaveHypaethralUserData

        "SIGN_IN" ->
            Just SignIn

        "SIGN_OUT" ->
            Just SignOut

        -----------------------------------------
        -- UI
        -----------------------------------------
        "HIDE_LOADING_SCREEN" ->
            Just HideLoadingScreen

        "LOAD_ENCLOSED_USER_DATA" ->
            Just LoadEnclosedUserData

        "LOAD_HYPAETHRAL_USER_DATA" ->
            Just LoadHypaethralUserData

        "REPORT_GENERIC_ERROR" ->
            Just ReportGenericError

        "REPORT_SOURCE_PROCESSING_ERROR" ->
            Just ReportSourceProcessingError

        _ ->
            Nothing
