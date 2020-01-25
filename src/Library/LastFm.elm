module LastFm exposing (..)

import Common
import Http
import Json.Decode as Json
import List.Ext as List
import MD5
import String.Ext as String
import Tracks exposing (IdentifiedTrack, Track)
import Tuple.Ext as Tuple
import Url exposing (Url)
import Url.Ext as Url



-- 🏔


apiKey =
    "4f0fe85b67baef8bb7d008a8754a95e5"


apiUrl =
    "http://ws.audioscrobbler.com/2.0"


notSoSecret =
    "0cec3ca0f58e04a5082f1131aba1e0d3"



-- 🌳


type alias Model =
    { authenticating : Bool
    , sessionKey : Maybe String
    }


initialModel : Url -> Model
initialModel url =
    case Url.action url of
        [ "authenticate", "lastfm" ] ->
            { authenticating = True
            , sessionKey = Nothing
            }

        _ ->
            { authenticating = False
            , sessionKey = Nothing
            }


authenticationCommand : (Result Http.Error String -> msg) -> Url -> Cmd msg
authenticationCommand msg url =
    case Url.action url of
        [ "authenticate", "lastfm" ] ->
            case Url.extractQueryParam "token" url of
                Just token ->
                    Http.get
                        { url =
                            authenticatedUrl
                                [ ( "method", "auth.getSession" )
                                , ( "token", token )
                                ]
                        , expect =
                            Json.string
                                |> Json.at [ "session", "key" ]
                                |> Http.expectJson msg
                        }

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none



-- 📣


disconnect : Model -> Model
disconnect model =
    { model | sessionKey = Nothing }


failedToAuthenticate : Model -> Model
failedToAuthenticate model =
    { model | authenticating = False }


gotSessionKey : String -> Model -> Model
gotSessionKey sessionKey model =
    { model | sessionKey = Just sessionKey }



-- 🎵


nowPlaying : Model -> IdentifiedTrack -> msg -> Cmd msg
nowPlaying model ( _, track ) msg =
    case model.sessionKey of
        Just sessionKey ->
            Http.post
                { url =
                    apiUrl
                , body =
                    authenticatedBody
                        [ ( "artist", track.tags.artist )
                        , ( "track", track.tags.title )
                        , ( "album", track.tags.album )
                        , ( "trackNumber", String.fromInt track.tags.nr )

                        --
                        , ( "method", "track.updateNowPlaying" )
                        , ( "sk", sessionKey )
                        ]
                , expect =
                    Http.expectWhatever (always msg)
                }

        Nothing ->
            Cmd.none


scrobble : Model -> Float -> Int -> Track -> msg -> Cmd msg
scrobble model duration timestamp track msg =
    case model.sessionKey of
        Just sessionKey ->
            Http.post
                { url =
                    apiUrl
                , body =
                    authenticatedBody
                        [ ( "artist", track.tags.artist )
                        , ( "track", track.tags.title )
                        , ( "album", track.tags.album )
                        , ( "trackNumber", String.fromInt track.tags.nr )

                        --
                        , ( "duration", String.fromInt <| round duration )
                        , ( "method", "track.scrobble" )
                        , ( "sk", sessionKey )
                        , ( "timestamp", String.fromInt timestamp )
                        ]
                , expect =
                    Http.expectWhatever (always msg)
                }

        Nothing ->
            Cmd.none



-- 🔱


authenticatedBody : List ( String, String ) -> Http.Body
authenticatedBody params =
    params
        |> authenticatedParams
        |> Common.queryString
        |> String.dropLeft 1
        |> Http.stringBody "application/x-www-form-urlencoded"


authenticatedUrl : List ( String, String ) -> String
authenticatedUrl params =
    params
        |> authenticatedParams
        |> Common.queryString
        |> String.append apiUrl


authenticatedParams : List ( String, String ) -> List ( String, String )
authenticatedParams params =
    let
        extendedParams =
            ( "api_key", apiKey ) :: params
    in
    extendedParams
        |> List.sortBy Tuple.first
        |> List.map (Tuple.uncurry String.append)
        |> String.concat
        |> String.addSuffix notSoSecret
        |> MD5.hex
        |> Tuple.pair "api_sig"
        |> List.addTo extendedParams
        |> (::) ( "format", "json" )
        |> List.sortBy Tuple.first
