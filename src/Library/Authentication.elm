module Authentication exposing (EnclosedUserData, HypaethralUserData, Method(..), decodeEnclosed, decodeHypaethral, decodeMethod, emptyHypaethralUserData, enclosedDecoder, encodeEnclosed, encodeHypaethral, encodeMethod, hypaethralDecoder, methodFromString, methodToString)

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
    = Ipfs { encryptionKey : String }
    | Local


type alias EnclosedUserData =
    { backgroundImage : Maybe String
    , onlyShowFavourites : Bool
    , repeat : Bool
    , searchTerm : Maybe String
    , shuffle : Bool
    , sortBy : Tracks.SortBy
    , sortDirection : Tracks.SortDirection
    }


type alias HypaethralUserData =
    { favourites : List Tracks.Favourite
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


methodToString : Method -> String
methodToString method =
    case method of
        Ipfs { encryptionKey } ->
            String.join methodSeparator [ "IPFS", encryptionKey ]

        Local ->
            "LOCAL"


methodFromString : String -> Maybe Method
methodFromString string =
    case String.split methodSeparator string of
        [ "IPFS", encryptionKey ] ->
            Just (Ipfs { encryptionKey = encryptionKey })

        [ "LOCAL" ] ->
            Just Local

        _ ->
            Nothing


methodSeparator : String
methodSeparator =
    ":::"



-- 🔱  ░░  ENCLOSED


decodeEnclosed : Json.Value -> Result Json.Error EnclosedUserData
decodeEnclosed =
    Json.decodeValue enclosedDecoder


enclosedDecoder : Json.Decoder EnclosedUserData
enclosedDecoder =
    Json.succeed EnclosedUserData
        |> required "backgroundImage" (Json.maybe Json.string)
        |> optional "onlyShowFavourites" Json.bool False
        |> optional "repeat" Json.bool False
        |> required "searchTerm" (Json.maybe Json.string)
        |> optional "shuffle" Json.bool False
        |> optional "sortBy" Tracks.sortByDecoder Tracks.Artist
        |> optional "sortDirection" Tracks.sortDirectionDecoder Tracks.Asc


encodeEnclosed : EnclosedUserData -> Json.Value
encodeEnclosed { backgroundImage, onlyShowFavourites, repeat, searchTerm, shuffle, sortBy, sortDirection } =
    Json.Encode.object
        [ ( "backgroundImage", Maybe.unwrap Json.Encode.null Json.Encode.string backgroundImage )
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
    , sources = []
    , tracks = []
    }


encodeHypaethral : HypaethralUserData -> Json.Value
encodeHypaethral { favourites, sources, tracks } =
    Json.Encode.object
        [ ( "favourites", Json.Encode.list Tracks.encodeFavourite favourites )
        , ( "sources", Json.Encode.list Sources.encode sources )
        , ( "tracks", Json.Encode.list Tracks.encodeTrack tracks )
        ]


hypaethralDecoder : Json.Decoder HypaethralUserData
hypaethralDecoder =
    Json.succeed HypaethralUserData
        |> optional "favourites" (Json.listIgnore Tracks.favouriteDecoder) []
        |> optional "sources" (Json.listIgnore Sources.decoder) []
        |> optional "tracks" (Json.listIgnore Tracks.trackDecoder) []
