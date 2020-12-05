module User.Layer exposing (..)

{-| User Layer.

This concerns data that relates to the app,
controlled by the user and stored by the user.

**Enclosed** data is data like, the enable-shuffle setting,
equalizer settings, or the currently-active-search term.
Which is stored in the browser.

**Hypaethral** data is data like, the user's favourites,
processed tracks, or the user's sources.
Which is stored in the location chosen by the user.

-}

import Dict exposing (Dict)
import Enum exposing (Enum)
import Equalizer
import Json.Decode as Json
import Json.Decode.Ext as Json
import Json.Decode.Pipeline exposing (optional)
import Json.Encode
import Maybe.Extra as Maybe
import Playlists
import Playlists.Encoding as Playlists
import Settings
import Sources
import Sources.Encoding as Sources
import Tracks
import Tracks.Encoding as Tracks



-- 🌳


type Method
    = Dropbox { token : String }
    | Fission
    | Ipfs { apiOrigin : String }
    | Local
    | RemoteStorage { userAddress : String, token : String }



-- 🌳  ░░  ENCLOSED


type alias EnclosedData =
    { cachedTracks : List String
    , equalizerSettings : Equalizer.Settings
    , grouping : Maybe Tracks.Grouping
    , onlyShowCachedTracks : Bool
    , onlyShowFavourites : Bool
    , repeat : Bool
    , scene : Tracks.Scene
    , searchTerm : Maybe String
    , selectedPlaylist : Maybe String
    , shuffle : Bool
    , sortBy : Tracks.SortBy
    , sortDirection : Tracks.SortDirection
    }



-- 🌳  ░░  HYPAETHRAL


type HypaethralBit
    = Favourites
    | Playlists
    | Progress
    | Settings
    | Sources
    | Tracks


type alias HypaethralData =
    { favourites : List Tracks.Favourite
    , playlists : List Playlists.Playlist
    , progress : Dict String Float
    , settings : Maybe Settings.Settings
    , sources : List Sources.Source
    , tracks : List Tracks.Track
    }



-- 🔱  ░░  METHOD


decodeMethod : Json.Value -> Maybe Method
decodeMethod =
    Json.decodeValue (Json.map methodFromString Json.string) >> Result.toMaybe >> Maybe.join


encodeMethod : Method -> Json.Value
encodeMethod =
    methodToString >> Json.Encode.string


methodFromString : String -> Maybe Method
methodFromString string =
    case String.split methodSeparator string of
        [ "DROPBOX", t ] ->
            Just (Dropbox { token = t })

        [ "FISSION" ] ->
            Just Fission

        [ "IPFS", a ] ->
            Just (Ipfs { apiOrigin = a })

        [ "LOCAL" ] ->
            Just Local

        [ "REMOTE_STORAGE", u, t ] ->
            Just (RemoteStorage { userAddress = u, token = t })

        _ ->
            Nothing


methodToString : Method -> String
methodToString method =
    case method of
        Dropbox { token } ->
            String.join
                methodSeparator
                [ "DROPBOX"
                , token
                ]

        Fission ->
            "FISSION"

        Ipfs { apiOrigin } ->
            String.join
                methodSeparator
                [ "IPFS"
                , apiOrigin
                ]

        Local ->
            "LOCAL"

        RemoteStorage { userAddress, token } ->
            String.join
                methodSeparator
                [ "REMOTE_STORAGE"
                , userAddress
                , token
                ]


methodSeparator : String
methodSeparator =
    "___"



-- 🔱  ░░  ENCLOSED


decodeEnclosedData : Json.Value -> Result Json.Error EnclosedData
decodeEnclosedData =
    Json.decodeValue enclosedDataDecoder


enclosedDataDecoder : Json.Decoder EnclosedData
enclosedDataDecoder =
    Json.succeed EnclosedData
        |> optional "cachedTracks" (Json.list Json.string) []
        |> optional "equalizerSettings" Equalizer.settingsDecoder Equalizer.defaultSettings
        |> optional "grouping" (Json.maybe Tracks.groupingDecoder) Nothing
        |> optional "onlyShowCachedTracks" Json.bool False
        |> optional "onlyShowFavourites" Json.bool False
        |> optional "repeat" Json.bool False
        |> optional "scene" Tracks.sceneDecoder Tracks.List
        |> optional "searchTerm" (Json.maybe Json.string) Nothing
        |> optional "selectedPlaylist" (Json.maybe Json.string) Nothing
        |> optional "shuffle" Json.bool False
        |> optional "sortBy" Tracks.sortByDecoder Tracks.Album
        |> optional "sortDirection" Tracks.sortDirectionDecoder Tracks.Asc


encodeEnclosedData : EnclosedData -> Json.Value
encodeEnclosedData { cachedTracks, equalizerSettings, grouping, onlyShowCachedTracks, onlyShowFavourites, repeat, scene, searchTerm, selectedPlaylist, shuffle, sortBy, sortDirection } =
    Json.Encode.object
        [ ( "cachedTracks", Json.Encode.list Json.Encode.string cachedTracks )
        , ( "equalizerSettings", Equalizer.encodeSettings equalizerSettings )
        , ( "grouping", Maybe.unwrap Json.Encode.null Tracks.encodeGrouping grouping )
        , ( "onlyShowCachedTracks", Json.Encode.bool onlyShowCachedTracks )
        , ( "onlyShowFavourites", Json.Encode.bool onlyShowFavourites )
        , ( "repeat", Json.Encode.bool repeat )
        , ( "scene", Tracks.encodeScene scene )
        , ( "searchTerm", Maybe.unwrap Json.Encode.null Json.Encode.string searchTerm )
        , ( "selectedPlaylist", Maybe.unwrap Json.Encode.null Json.Encode.string selectedPlaylist )
        , ( "shuffle", Json.Encode.bool shuffle )
        , ( "sortBy", Tracks.encodeSortBy sortBy )
        , ( "sortDirection", Tracks.encodeSortDirection sortDirection )
        ]



-- 🔱  ░░  HYPAETHRAL


decodeHypaethralData : Json.Value -> Result Json.Error HypaethralData
decodeHypaethralData =
    Json.decodeValue hypaethralDataDecoder


emptyHypaethralData : HypaethralData
emptyHypaethralData =
    { favourites = []
    , playlists = []
    , progress = Dict.empty
    , settings = Nothing
    , sources = []
    , tracks = []
    }


encodeHypaethralBit : HypaethralBit -> HypaethralData -> Json.Value
encodeHypaethralBit bit { favourites, playlists, progress, settings, sources, tracks } =
    case bit of
        Favourites ->
            Json.Encode.list Tracks.encodeFavourite favourites

        Playlists ->
            Json.Encode.list Playlists.encode playlists

        Progress ->
            Json.Encode.dict identity Json.Encode.float progress

        Settings ->
            Maybe.unwrap Json.Encode.null Settings.encode settings

        Sources ->
            Json.Encode.list Sources.encode sources

        Tracks ->
            Json.Encode.list Tracks.encodeTrack tracks


encodeHypaethralData : HypaethralData -> Json.Value
encodeHypaethralData data =
    Json.Encode.object
        [ ( hypaethralBitKey Favourites, encodeHypaethralBit Favourites data )
        , ( hypaethralBitKey Playlists, encodeHypaethralBit Playlists data )
        , ( hypaethralBitKey Progress, encodeHypaethralBit Progress data )
        , ( hypaethralBitKey Settings, encodeHypaethralBit Settings data )
        , ( hypaethralBitKey Sources, encodeHypaethralBit Sources data )
        , ( hypaethralBitKey Tracks, encodeHypaethralBit Tracks data )
        ]


hypaethralBit : Enum HypaethralBit
hypaethralBit =
    Enum.create
        [ ( hypaethralBitKey Favourites, Favourites )
        , ( hypaethralBitKey Playlists, Playlists )
        , ( hypaethralBitKey Progress, Progress )
        , ( hypaethralBitKey Settings, Settings )
        , ( hypaethralBitKey Sources, Sources )
        , ( hypaethralBitKey Tracks, Tracks )
        ]


hypaethralBitFileName : HypaethralBit -> String
hypaethralBitFileName bit =
    hypaethralBitKey bit ++ ".json"


hypaethralBitKey : HypaethralBit -> String
hypaethralBitKey bit =
    case bit of
        Favourites ->
            "favourites"

        Playlists ->
            "playlists"

        Progress ->
            "progress"

        Settings ->
            "settings"

        Sources ->
            "sources"

        Tracks ->
            "tracks"


hypaethralDataDecoder : Json.Decoder HypaethralData
hypaethralDataDecoder =
    Json.succeed HypaethralData
        |> optional (hypaethralBitKey Favourites) (Json.listIgnore Tracks.favouriteDecoder) []
        |> optional (hypaethralBitKey Playlists) (Json.listIgnore Playlists.decoder) []
        |> optional (hypaethralBitKey Progress) (Json.dict Json.float) Dict.empty
        |> optional (hypaethralBitKey Settings) (Json.maybe Settings.decoder) Nothing
        |> optional (hypaethralBitKey Sources) (Json.listIgnore Sources.decoder) []
        |> optional (hypaethralBitKey Tracks) (Json.listIgnore Tracks.trackDecoder) []


putHypaethralJsonBitsTogether : List ( HypaethralBit, Json.Value ) -> Json.Value
putHypaethralJsonBitsTogether bits =
    bits
        |> List.map (Tuple.mapFirst hypaethralBitKey)
        |> Json.Encode.object
