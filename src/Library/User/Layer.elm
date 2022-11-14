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
import List.Extra as List
import Maybe.Extra as Maybe
import Playlists
import Playlists.Encoding as Playlists
import Settings
import Sources
import Sources.Encoding as Sources
import Time
import Time.Ext as Time
import Tracks
import Tracks.Encoding as Tracks



-- 🌳


type Method
    = Dropbox { accessToken : String, expiresAt : Int, refreshToken : String }
    | Fission { initialised : Bool }
    | Ipfs { apiOrigin : String }
    | Local
    | RemoteStorage { userAddress : String, token : String }


methodSupportsPublicData : Method -> Bool
methodSupportsPublicData method =
    case method of
        Dropbox _ ->
            False

        Fission _ ->
            -- NOTE: Temporarily disabled,
            --       since we don't actually support public playlists yet.
            False

        Ipfs _ ->
            False

        Local ->
            False

        RemoteStorage _ ->
            False



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


type HypaethralBaggage
    = BaggageClaimed
      --
    | PlaylistsBaggage PlaylistsBaggageAttributes


type alias HypaethralData =
    { favourites : List Tracks.Favourite
    , playlists : List Playlists.Playlist
    , progress : Dict String Float
    , settings : Maybe Settings.Settings
    , sources : List Sources.Source
    , tracks : List Tracks.Track

    --
    , modifiedAt : Maybe Time.Posix
    }


type alias PlaylistsBaggageAttributes =
    { publicPlaylistsRead : List Json.Value
    , publicPlaylistsTodo : List String
    , privatePlaylistsRead : List Json.Value
    , privatePlaylistsTodo : List String
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
        [ "DROPBOX", a, e, r ] ->
            Just
                (Dropbox
                    { accessToken = a
                    , expiresAt = Maybe.withDefault 0 (String.toInt e)
                    , refreshToken = r
                    }
                )

        [ "FISSION" ] ->
            Just (Fission { initialised = False })

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
        Dropbox { accessToken, expiresAt, refreshToken } ->
            String.join
                methodSeparator
                [ "DROPBOX"
                , accessToken
                , String.fromInt expiresAt
                , refreshToken
                ]

        Fission _ ->
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
        |> optional "scene" Tracks.sceneDecoder Tracks.Covers
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

    --
    , modifiedAt = Nothing
    }


encodeHypaethralBit : HypaethralBit -> HypaethralData -> Json.Value
encodeHypaethralBit bit { favourites, playlists, progress, settings, sources, tracks, modifiedAt } =
    Json.Encode.object
        [ ( "data"
          , case bit of
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
          )
        , ( "modifiedAt"
          , Maybe.unwrap Json.Encode.null Time.encode modifiedAt
          )
        ]


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
    let
        optionalWithPossiblyData key dec def a =
            optional
                (hypaethralBitKey key)
                (Json.oneOf [ modifiedAtDecoder dec, noModifiedAt dec ])
                { data = def, modifiedAt = Nothing }
                a
    in
    (\fav pla pro set sor tra ->
        { favourites = fav.data
        , playlists = pla.data
        , progress = pro.data
        , settings = set.data
        , sources = sor.data
        , tracks = tra.data

        --
        , modifiedAt =
            [ fav.modifiedAt
            , pla.modifiedAt
            , pro.modifiedAt
            , set.modifiedAt
            , sor.modifiedAt
            , tra.modifiedAt
            ]
                |> List.filterMap (Maybe.map Time.posixToMillis)
                |> List.sort
                |> List.last
                |> Maybe.map Time.millisToPosix
        }
    )
        |> Json.succeed
        |> optionalWithPossiblyData Favourites (Json.listIgnore Tracks.favouriteDecoder) []
        |> optionalWithPossiblyData Playlists (Json.listIgnore Playlists.decoder) []
        |> optionalWithPossiblyData Progress (Json.dict Json.float) Dict.empty
        |> optionalWithPossiblyData Settings (Json.maybe Settings.decoder) Nothing
        |> optionalWithPossiblyData Sources (Json.listIgnore Sources.decoder) []
        |> optionalWithPossiblyData Tracks (Json.listIgnore Tracks.trackDecoder) []



--


modifiedAtDecoder : Json.Decoder a -> Json.Decoder { data : a, modifiedAt : Maybe Time.Posix }
modifiedAtDecoder decoder =
    Json.map2
        (\d m -> { data = d, modifiedAt = m })
        (Json.field "data" decoder)
        (Json.maybe <| Json.field "modifiedAt" Time.decoder)


noModifiedAt : Json.Decoder a -> Json.Decoder { data : a, modifiedAt : Maybe Time.Posix }
noModifiedAt =
    Json.map
        (\data ->
            { data = data
            , modifiedAt = Nothing
            }
        )


putHypaethralJsonBitsTogether : List ( HypaethralBit, Json.Value, HypaethralBaggage ) -> Json.Value
putHypaethralJsonBitsTogether bits =
    bits
        |> List.map (\( a, b, _ ) -> ( hypaethralBitKey a, b ))
        |> Json.Encode.object



-- 🔱  ░░  BAGGAGE


mapPlaylistsBaggage : (PlaylistsBaggageAttributes -> PlaylistsBaggageAttributes) -> HypaethralBaggage -> HypaethralBaggage
mapPlaylistsBaggage fn baggage =
    case baggage of
        PlaylistsBaggage p ->
            PlaylistsBaggage (fn p)

        b ->
            b
