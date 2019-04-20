module Alien exposing (Event, Tag(..), broadcast, hostDecoder, report, tagFromString, tagToString, trigger)

{-| 👽 Aliens.

This involves the incoming and outgoing data.
Including the communication between the different Elm apps/workers.

-}

import Json.Decode
import Json.Encode



-- 🌳


type alias Event =
    { tag : String, data : Json.Encode.Value, error : Maybe String }


type Tag
    = AuthAnonymous
    | AuthEnclosedData
    | AuthIpfs
    | AuthMethod
    | AuthRemoteStorage
    | AuthSecretKey
    | AuthTextile
    | FabricateSecretKey
    | SearchTracks
      -- from UI
    | ProcessSources
    | SaveEnclosedUserData
    | SaveFavourites
    | SaveSettings
    | SaveSources
    | SaveTracks
    | SignIn
    | SignOut
    | ToCache
    | UpdateEncryptionKey
      -- to UI
    | AddTracks
    | FinishedProcessingSources
    | HideLoadingScreen
    | LoadEnclosedUserData
    | LoadHypaethralUserData
    | NotAuthenticated
    | RemoveTracksByPath
    | ReportProcessingError
    | UpdateSourceData



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

        AuthEnclosedData ->
            "AUTH_ENCLOSED_DATA"

        AuthIpfs ->
            "AUTH_IPFS"

        AuthMethod ->
            "AUTH_METHOD"

        AuthRemoteStorage ->
            "AUTH_REMOTE_STORAGE"

        AuthSecretKey ->
            "AUTH_SECRET_KEY"

        AuthTextile ->
            "AUTH_TEXTILE"

        FabricateSecretKey ->
            "FABRICATE_SECRET_KEY"

        SearchTracks ->
            "SEARCH_TRACKS"

        -----------------------------------------
        -- From UI
        -----------------------------------------
        ProcessSources ->
            "PROCESS_SOURCES"

        SaveEnclosedUserData ->
            "SAVE_ENCLOSED_USER_DATA"

        SaveFavourites ->
            "SAVE_FAVOURITES"

        SaveSettings ->
            "SAVE_SETTINGS"

        SaveSources ->
            "SAVE_SOURCES"

        SaveTracks ->
            "SAVE_TRACKS"

        SignIn ->
            "SIGN_IN"

        SignOut ->
            "SIGN_OUT"

        ToCache ->
            "TO_CACHE"

        UpdateEncryptionKey ->
            "UPDATE_ENCRYPTION_KEY"

        -----------------------------------------
        -- To UI
        -----------------------------------------
        AddTracks ->
            "ADD_TRACKS"

        FinishedProcessingSources ->
            "FINISHED_PROCESSING_SOURCES"

        HideLoadingScreen ->
            "HIDE_LOADING_SCREEN"

        LoadEnclosedUserData ->
            "LOAD_ENCLOSED_USER_DATA"

        LoadHypaethralUserData ->
            "LOAD_HYPAETHRAL_USER_DATA"

        NotAuthenticated ->
            "NOT_AUTHENTICATED"

        RemoveTracksByPath ->
            "REMOVE_TRACKS_BY_PATH"

        ReportProcessingError ->
            "REPORT_PROCESSING_ERROR"

        UpdateSourceData ->
            "UPDATE_SOURCE_DATA"


tagFromString : String -> Maybe Tag
tagFromString string =
    case string of
        "AUTH_ANONYMOUS" ->
            Just AuthAnonymous

        "AUTH_ENCLOSED_DATA" ->
            Just AuthEnclosedData

        "AUTH_IPFS" ->
            Just AuthIpfs

        "AUTH_METHOD" ->
            Just AuthMethod

        "AUTH_REMOTE_STORAGE" ->
            Just AuthRemoteStorage

        "AUTH_SECRET_KEY" ->
            Just AuthSecretKey

        "AUTH_TEXTILE" ->
            Just AuthTextile

        "FABRICATE_SECRET_KEY" ->
            Just FabricateSecretKey

        "SEARCH_TRACKS" ->
            Just SearchTracks

        -----------------------------------------
        -- From UI
        -----------------------------------------
        "PROCESS_SOURCES" ->
            Just ProcessSources

        "SAVE_ENCLOSED_USER_DATA" ->
            Just SaveEnclosedUserData

        "SAVE_FAVOURITES" ->
            Just SaveFavourites

        "SAVE_SETTINGS" ->
            Just SaveSettings

        "SAVE_SOURCES" ->
            Just SaveSources

        "SAVE_TRACKS" ->
            Just SaveTracks

        "SIGN_IN" ->
            Just SignIn

        "SIGN_OUT" ->
            Just SignOut

        "TO_CACHE" ->
            Just ToCache

        "UPDATE_ENCRYPTION_KEY" ->
            Just UpdateEncryptionKey

        -----------------------------------------
        -- UI
        -----------------------------------------
        "ADD_TRACKS" ->
            Just AddTracks

        "FINISHED_PROCESSING_SOURCES" ->
            Just FinishedProcessingSources

        "HIDE_LOADING_SCREEN" ->
            Just HideLoadingScreen

        "LOAD_ENCLOSED_USER_DATA" ->
            Just LoadEnclosedUserData

        "LOAD_HYPAETHRAL_USER_DATA" ->
            Just LoadHypaethralUserData

        "NOT_AUTHENTICATED" ->
            Just NotAuthenticated

        "REMOVE_TRACKS_BY_PATH" ->
            Just RemoveTracksByPath

        "REPORT_PROCESSING_ERROR" ->
            Just ReportProcessingError

        "UPDATE_SOURCE_DATA" ->
            Just UpdateSourceData

        _ ->
            Nothing



-- ⚗️


{-| Decoder for an alien event inside another alient event.
-}
hostDecoder : Json.Decode.Decoder Event
hostDecoder =
    Json.Decode.map3
        (\tag data error ->
            { tag = tag
            , data = data
            , error = error
            }
        )
        (Json.Decode.field "tag" Json.Decode.string)
        (Json.Decode.field "data" Json.Decode.value)
        (Json.Decode.field "error" <| Json.Decode.maybe Json.Decode.string)
