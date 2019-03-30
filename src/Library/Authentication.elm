module Authentication exposing (EnclosedUserData, HypaethralUserData, Method(..), Settings, decodeEnclosed, decodeHypaethral, decodeMethod, emptyHypaethralUserData, enclosedDecoder, encodeEnclosed, encodeHypaethral, encodeMethod, encodeSettings, hypaethralDecoder, methodFromString, methodToString, settingsDecoder)

import Equalizer
import Json.Decode as Json
import Json.Decode.Ext as Json
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Maybe.Extra as Maybe
import Sources
import Sources.Encoding as Sources
import Tracks
import Tracks.Encoding as Tracks



-- 🌳


type Method
    = Ipfs
    | Local


type alias EnclosedUserData =
    { equalizerSettings : Equalizer.Settings
    , onlyShowFavourites : Bool
    , repeat : Bool
    , searchTerm : Maybe String
    , shuffle : Bool
    , sortBy : Tracks.SortBy
    , sortDirection : Tracks.SortDirection
    }


type alias HypaethralUserData =
    { favourites : List Tracks.Favourite
    , settings : Maybe Settings
    , sources : List Sources.Source
    , tracks : List Tracks.Track
    }


type alias Settings =
    { backgroundImage : Maybe String
    , hideDuplicates : Bool
    }



-- 🔱  ░░  METHOD


decodeMethod : Json.Value -> Maybe Method
decodeMethod =
    Json.decodeValue (Json.map methodFromString Json.string) >> Result.toMaybe >> Maybe.join


encodeMethod : Method -> Json.Value
encodeMethod =
    methodToString >> Json.Encode.string


methodToString : Method -> String
methodToString method =
    case method of
        Ipfs ->
            "IPFS"

        Local ->
            "LOCAL"


methodFromString : String -> Maybe Method
methodFromString string =
    case string of
        "IPFS" ->
            Just Ipfs

        "LOCAL" ->
            Just Local

        _ ->
            Nothing



-- 🔱  ░░  ENCLOSED


decodeEnclosed : Json.Value -> Result Json.Error EnclosedUserData
decodeEnclosed =
    Json.decodeValue enclosedDecoder


enclosedDecoder : Json.Decoder EnclosedUserData
enclosedDecoder =
    Json.succeed EnclosedUserData
        |> optional "equalizerSettings" Equalizer.settingsDecoder Equalizer.defaultSettings
        |> optional "onlyShowFavourites" Json.bool False
        |> optional "repeat" Json.bool False
        |> optional "searchTerm" (Json.maybe Json.string) Nothing
        |> optional "shuffle" Json.bool False
        |> optional "sortBy" Tracks.sortByDecoder Tracks.Artist
        |> optional "sortDirection" Tracks.sortDirectionDecoder Tracks.Asc


encodeEnclosed : EnclosedUserData -> Json.Value
encodeEnclosed { equalizerSettings, onlyShowFavourites, repeat, searchTerm, shuffle, sortBy, sortDirection } =
    Json.Encode.object
        [ ( "equalizerSettings", Equalizer.encodeSettings equalizerSettings )
        , ( "onlyShowFavourites", Json.Encode.bool onlyShowFavourites )
        , ( "repeat", Json.Encode.bool repeat )
        , ( "searchTerm", Maybe.unwrap Json.Encode.null Json.Encode.string searchTerm )
        , ( "shuffle", Json.Encode.bool shuffle )
        , ( "sortBy", Tracks.encodeSortBy sortBy )
        , ( "sortDirection", Tracks.encodeSortDirection sortDirection )
        ]



-- 🔱  ░░  HYPAETHRAL


decodeHypaethral : Json.Value -> Result Json.Error HypaethralUserData
decodeHypaethral =
    Json.decodeValue hypaethralDecoder


emptyHypaethralUserData : HypaethralUserData
emptyHypaethralUserData =
    { favourites = []
    , settings = Nothing
    , sources = []
    , tracks = []
    }


encodeHypaethral : HypaethralUserData -> Json.Value
encodeHypaethral { favourites, settings, sources, tracks } =
    Json.Encode.object
        [ ( "favourites", Json.Encode.list Tracks.encodeFavourite favourites )
        , ( "settings", Maybe.unwrap Json.Encode.null encodeSettings settings )
        , ( "sources", Json.Encode.list Sources.encode sources )
        , ( "tracks", Json.Encode.list Tracks.encodeTrack tracks )
        ]


encodeSettings : Settings -> Json.Value
encodeSettings settings =
    Json.Encode.object
        [ ( "backgroundImage"
          , Maybe.unwrap Json.Encode.null Json.Encode.string settings.backgroundImage
          )
        , ( "hideDuplicates"
          , Json.Encode.bool settings.hideDuplicates
          )
        ]


hypaethralDecoder : Json.Decoder HypaethralUserData
hypaethralDecoder =
    Json.succeed HypaethralUserData
        |> optional "favourites" (Json.listIgnore Tracks.favouriteDecoder) []
        |> optional "settings" (Json.maybe settingsDecoder) Nothing
        |> optional "sources" (Json.listIgnore Sources.decoder) []
        |> optional "tracks" (Json.listIgnore Tracks.trackDecoder) []


settingsDecoder : Json.Decoder Settings
settingsDecoder =
    Json.succeed Settings
        |> optional "backgroundImage" (Json.maybe Json.string) Nothing
        |> optional "hideDuplicates" Json.bool False
