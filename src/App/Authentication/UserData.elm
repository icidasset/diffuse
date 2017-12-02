module Authentication.UserData exposing (..)

import Authentication.Types exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as TopLevel


-- Children, Pt. 1

import Playlists.Encoding as Playlists
import Sources.Encoding as Sources
import Tracks.Encoding as Tracks


-- Children, Pt. 2

import Equalizer.Types
import Playlists.Types
import Queue.Types
import Settings.Types
import Sources.Types
import Sources.Utils exposing (pickEnableSourceIds)
import Tracks.Types


-- 🌖


type alias Bundle =
    ( UserData, Decode.Value )


inwards : String -> TopLevel.Model -> TopLevel.Model
inwards json model =
    let
        data =
            Result.withDefault emptyUserData (decode json)

        bundle =
            ( data
            , data.settings |> Maybe.withDefault Encode.null
            )
    in
        model
            |> (\m -> { m | equalizer = importEqualizer m.equalizer bundle })
            |> (\m -> { m | playlists = importPlaylists m.playlists bundle })
            |> (\m -> { m | queue = importQueue m.queue bundle })
            |> (\m -> { m | settings = importSettings m.settings bundle })
            |> (\m -> { m | sources = importSources m.sources bundle })
            |> (\m -> { m | tracks = importTracks m.tracks bundle })



--


importEqualizer : Equalizer.Types.Model -> Bundle -> Equalizer.Types.Model
importEqualizer pre ( _, obj ) =
    let
        coder =
            decodeSetting obj "equalizer"
    in
        { pre
            | low = coder "low" Decode.float pre.low
            , mid = coder "mid" Decode.float pre.mid
            , high = coder "high" Decode.float pre.high
            , volume = coder "volume" Decode.float pre.volume
        }


importPlaylists : Playlists.Types.Model -> Bundle -> Playlists.Types.Model
importPlaylists pre ( data, _ ) =
    { pre
        | collection = Maybe.withDefault [] data.playlists
    }


importQueue : Queue.Types.Model -> Bundle -> Queue.Types.Model
importQueue pre ( _, obj ) =
    let
        coder =
            decodeSetting obj "queue"
    in
        { pre
            | repeat = coder "repeat" Decode.bool pre.repeat
            , shuffle = coder "shuffle" Decode.bool pre.shuffle
        }


importSettings : Settings.Types.Model -> Bundle -> Settings.Types.Model
importSettings pre ( _, obj ) =
    let
        coder =
            decodeSetting obj "application"
    in
        { pre
            | backgroundImage = coder "backgroundImage" Decode.string pre.backgroundImage
        }


importSources : Sources.Types.Model -> Bundle -> Sources.Types.Model
importSources pre ( data, _ ) =
    { pre
        | collection = Maybe.withDefault [] data.sources
    }


importTracks : Tracks.Types.Model -> Bundle -> Tracks.Types.Model
importTracks pre ( data, obj ) =
    let
        coder =
            decodeSetting obj "tracks"

        col =
            pre.collection
    in
        { pre
            | collection =
                Maybe.withDefault [] data.tracks |> (\l -> { col | untouched = l })
            , enabledSourceIds =
                Maybe.withDefault [] data.sources |> pickEnableSourceIds
            , favourites =
                Maybe.withDefault [] data.favourites
            , favouritesOnly =
                coder "favouritesOnly"
                    Decode.bool
                    pre.favouritesOnly
            , searchTerm =
                coder "searchTerm"
                    (Decode.maybe Decode.string)
                    pre.searchTerm
            , selectedPlaylist =
                coder "selectedPlaylist"
                    (Decode.maybe Playlists.decoder)
                    pre.selectedPlaylist
        }



--


emptyUserData : UserData
emptyUserData =
    { favourites = Nothing
    , playlists = Nothing
    , settings = Nothing
    , sources = Nothing
    , tracks = Nothing
    }



-- In / Decoding


decode : String -> Result String UserData
decode =
    Decode.decodeString decoder


decoder : Decode.Decoder UserData
decoder =
    Decode.map5
        UserData
        (Decode.maybe <| Decode.field "favourites" <| Decode.list Tracks.favouriteDecoder)
        (Decode.maybe <| Decode.field "playlists" <| Decode.list Playlists.decoder)
        (Decode.maybe <| Decode.field "settings" Decode.value)
        (Decode.maybe <| Decode.field "sources" <| Decode.list Sources.decoder)
        (Decode.maybe <| Decode.field "tracks" <| Decode.list Tracks.trackDecoder)


decodeSetting : Decode.Value -> String -> String -> Decode.Decoder x -> x -> x
decodeSetting value a b decoder fallback =
    let
        upgradedDecoder =
            decoder
                |> Decode.at [ a, b ]
                |> Decode.maybe
                |> Decode.map (Maybe.withDefault fallback)
    in
        value
            |> Decode.decodeValue upgradedDecoder
            |> Result.withDefault fallback



-- 🌒


outwards : TopLevel.Model -> String
outwards =
    encode



-- Out / Encoding


encode : TopLevel.Model -> String
encode model =
    let
        favourites =
            model.tracks.favourites
                |> List.map Tracks.encodeFavourite
                |> Encode.list

        playlists =
            model.playlists.collection
                |> List.filter (.autoGenerated >> (==) False)
                |> List.map Playlists.encode
                |> Encode.list

        sources =
            model.sources.collection
                |> List.map Sources.encode
                |> Encode.list

        tracks =
            model.tracks.collection.untouched
                |> List.map Tracks.encodeTrack
                |> Encode.list

        settings =
            encodeSettings model
    in
        [ ( "favourites", favourites )
        , ( "playlists", playlists )
        , ( "settings", settings )
        , ( "sources", sources )
        , ( "tracks", tracks )
        ]
            |> Encode.object
            |> Encode.encode 4


encodeSettings : TopLevel.Model -> Encode.Value
encodeSettings model =
    let
        application =
            Encode.object
                [ ( "backgroundImage", Encode.string model.settings.backgroundImage )
                ]

        equalizer =
            Encode.object
                [ ( "low", Encode.float model.equalizer.low )
                , ( "mid", Encode.float model.equalizer.mid )
                , ( "high", Encode.float model.equalizer.high )
                , ( "volume", Encode.float model.equalizer.volume )
                ]

        queue =
            Encode.object
                [ ( "repeat", Encode.bool model.queue.repeat )
                , ( "shuffle", Encode.bool model.queue.shuffle )
                ]

        tracks =
            Encode.object
                [ ( "favouritesOnly", Encode.bool model.tracks.favouritesOnly )
                , ( "searchTerm"
                  , case model.tracks.searchTerm of
                        Just string ->
                            Encode.string string

                        Nothing ->
                            Encode.null
                  )
                , ( "selectedPlaylist"
                  , case model.tracks.selectedPlaylist of
                        Just playlist ->
                            Playlists.encode playlist

                        Nothing ->
                            Encode.null
                  )
                ]
    in
        Encode.object
            [ ( "application", application )
            , ( "equalizer", equalizer )
            , ( "queue", queue )
            , ( "tracks", tracks )
            ]
