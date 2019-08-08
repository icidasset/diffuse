module Sources.Services.Google.Parser exposing (fileDecoder, parseErrorResponse, parsePreparationResponse, parseTreeResponse)

import Dict
import Json.Decode exposing (..)
import Maybe.Extra
import Sources exposing (SourceData)
import Sources.Pick
import Sources.Processing exposing (Marker(..), PrepationAnswer, TreeAnswer)



-- PREPARATION


parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse response srcData _ =
    let
        newAccessToken =
            response
                |> decodeString (field "access_token" string)
                |> Result.withDefault ""

        maybeRefreshToken =
            response
                |> decodeString (maybe <| field "refresh_token" string)
                |> Result.toMaybe
                |> Maybe.Extra.join

        refreshTokenUpdater dict =
            case maybeRefreshToken of
                Just refreshToken ->
                    Dict.insert "refreshToken" refreshToken dict

                Nothing ->
                    dict
    in
    srcData
        |> Dict.insert "accessToken" newAccessToken
        |> refreshTokenUpdater
        |> Dict.remove "authCode"
        |> (\s -> { sourceData = s, marker = TheEnd })



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response _ =
    let
        nextPageToken =
            response
                |> decodeString (maybe <| field "nextPageToken" string)
                |> Result.toMaybe
                |> Maybe.Extra.join

        files =
            decodeString
                (field "files" <| list fileDecoder)
                response
    in
    { filePaths =
        files
            |> Result.withDefault []
            |> List.filter (Tuple.second >> Sources.Pick.isMusicFile)
            |> List.map Tuple.first
    , marker =
        case nextPageToken of
            Just token ->
                InProgress token

            Nothing ->
                TheEnd
    }


fileDecoder : Decoder ( String, String )
fileDecoder =
    map2 Tuple.pair
        (field "id" string)
        (field "name" string)



-- ERROR


parseErrorResponse : String -> Maybe String
parseErrorResponse response =
    response
        |> decodeString (at [ "error", "message" ] string)
        |> Result.toMaybe
