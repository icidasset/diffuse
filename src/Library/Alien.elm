module Alien exposing (Event, Tag(..), broadcast, hostDecoder, report, tagDecoder, tagFromJson, tagFromString, tagToJson, tagToString, trigger)

{-| 👽 Aliens.

This involves the incoming and outgoing data.
Including the communication between the different Elm apps/workers.

-}

import Enum exposing (Enum)
import Json.Decode
import Json.Encode



-- 🌳


type alias Event =
    { tag : String
    , data : Json.Encode.Value
    , error : Maybe String
    }


type Tag
    = AuthAnonymous
    | AuthDropbox
    | AuthFission
    | AuthEnclosedData
    | AuthIpfs
    | AuthMethod
    | AuthRemoteStorage
    | AuthSecretKey
    | FabricateSecretKey
    | SearchTracks
      -----------------------------------------
      -- from UI
      -----------------------------------------
    | DownloadTracks
    | ImportLegacyData
    | ProcessSources
    | RemoveEncryptionKey
    | RemoveTracksBySourceId
    | RemoveTracksFromCache
    | SaveEnclosedUserData
    | SaveFavourites
    | SavePlaylists
    | SaveProgress
    | SaveSettings
    | SaveSources
    | SaveTracks
    | SignIn
    | SignOut
    | StopProcessing
    | StoreTracksInCache
    | SyncHypaethralData
    | SyncTrackTags
    | ToCache
    | UpdateEncryptionKey
      -----------------------------------------
      -- to UI
      -----------------------------------------
    | AddTracks
    | FinishedProcessingSource
    | FinishedProcessingSources
    | GotCachedCover
    | HideLoadingScreen
    | LoadEnclosedUserData
    | LoadHypaethralUserData
    | MissingSecretKey
    | NotAuthenticated
    | ReloadTracks
    | RemoveTracksByPath
    | ReportProcessingError
    | ReportProcessingProgress
    | UpdateSourceData


enum : Enum Tag
enum =
    Enum.create
        [ ( "AUTH_ANONYMOUS", AuthAnonymous )
        , ( "AUTH_DROPBOX", AuthDropbox )
        , ( "AUTH_ENCLOSED_DATA", AuthEnclosedData )
        , ( "AUTH_FISSION", AuthFission )
        , ( "AUTH_IPFS", AuthIpfs )
        , ( "AUTH_METHOD", AuthMethod )
        , ( "AUTH_REMOTE_STORAGE", AuthRemoteStorage )
        , ( "AUTH_SECRET_KEY", AuthSecretKey )
        , ( "FABRICATE_SECRET_KEY", FabricateSecretKey )
        , ( "SEARCH_TRACKS", SearchTracks )

        -----------------------------------------
        -- From UI
        -----------------------------------------
        , ( "DOWNLOAD_TRACKS", DownloadTracks )
        , ( "IMPORT_LEGACY_DATA", ImportLegacyData )
        , ( "PROCESS_SOURCES", ProcessSources )
        , ( "REMOVE_ENCRYPTION_KEY", RemoveEncryptionKey )
        , ( "REMOVE_TRACKS_BY_SOURCE_ID", RemoveTracksBySourceId )
        , ( "REMOVE_TRACKS_FROM_CACHE", RemoveTracksFromCache )
        , ( "SAVE_ENCLOSED_USER_DATA", SaveEnclosedUserData )
        , ( "SAVE_FAVOURITES", SaveFavourites )
        , ( "SAVE_PLAYLISTS", SavePlaylists )
        , ( "SAVE_PROGRESS", SaveProgress )
        , ( "SAVE_SETTINGS", SaveSettings )
        , ( "SAVE_SOURCES", SaveSources )
        , ( "SAVE_TRACKS", SaveTracks )
        , ( "SIGN_IN", SignIn )
        , ( "SIGN_OUT", SignOut )
        , ( "STOP_PROCESSING", StopProcessing )
        , ( "STORE_TRACKS_IN_CACHE", StoreTracksInCache )
        , ( "SYNC_HYPAETHRAL_DATA", SyncHypaethralData )
        , ( "SYNC_TRACK_TAGS", SyncTrackTags )
        , ( "TO_CACHE", ToCache )
        , ( "UPDATE_ENCRYPTION_KEY", UpdateEncryptionKey )

        -----------------------------------------
        -- To UI
        -----------------------------------------
        , ( "ADD_TRACKS", AddTracks )
        , ( "FINISHED_PROCESSING_SOURCE", FinishedProcessingSource )
        , ( "FINISHED_PROCESSING_SOURCES", FinishedProcessingSources )
        , ( "GOT_CACHED_COVER", GotCachedCover )
        , ( "HIDE_LOADING_SCREEN", HideLoadingScreen )
        , ( "LOAD_ENCLOSED_USER_DATA", LoadEnclosedUserData )
        , ( "LOAD_HYPAETHRAL_USER_DATA", LoadHypaethralUserData )
        , ( "MISSING_SECRET_KEY", MissingSecretKey )
        , ( "NOT_AUTHENTICATED", NotAuthenticated )
        , ( "RELOAD_TRACKS", ReloadTracks )
        , ( "REMOVE_TRACKS_BY_PATH", RemoveTracksByPath )
        , ( "REPORT_PROCESSING_ERROR", ReportProcessingError )
        , ( "REPORT_PROCESSING_PROGRESS", ReportProcessingProgress )
        , ( "UPDATE_SOURCE_DATA", UpdateSourceData )
        ]



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


tagDecoder : Json.Decode.Decoder Tag
tagDecoder =
    enum.decoder


tagToJson : Tag -> Json.Encode.Value
tagToJson =
    enum.encode


tagToString : Tag -> String
tagToString =
    enum.toString


tagFromJson : Json.Decode.Value -> Result Json.Decode.Error Tag
tagFromJson =
    Json.Decode.decodeValue enum.decoder


tagFromString : String -> Maybe Tag
tagFromString =
    enum.fromString



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
