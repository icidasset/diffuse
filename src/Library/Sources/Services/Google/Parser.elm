module Sources.Services.Google.Parser exposing (..)

import Dict
import Json.Decode exposing (..)
import Json.Decode.Ext exposing (..)
import Maybe.Extra
import Sources exposing (SourceData)
import Sources.Pick
import Sources.Processing exposing (Marker(..), PrepationAnswer, TreeAnswer)
import Sources.Services.Google.Marker as Marker
import String.Path



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


type alias Properties =
    { id : String, name : String }


type Item
    = File Properties
    | Directory Properties


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        nextPageToken =
            response
                |> decodeString (maybe <| field "nextPageToken" string)
                |> Result.toMaybe
                |> Maybe.Extra.join

        items =
            response
                |> decodeString (field "files" <| listIgnore itemDecoder)
                |> Result.withDefault []

        usedDirectory =
            previousMarker
                |> Marker.takeOne
                |> Maybe.map Marker.itemDirectory
                |> Maybe.withDefault ""

        usedPath =
            usedDirectory
                |> String.Path.dropRight 1
                |> String.Path.addSuffix

        ( directories, files ) =
            List.partition
                (\item ->
                    case item of
                        Directory _ ->
                            True

                        File _ ->
                            False
                )
                items
    in
    { filePaths =
        files
            |> List.map itemProperties
            |> List.filter (.name >> Sources.Pick.isMusicFile)
            |> List.map (\{ id, name } -> usedPath ++ id ++ "?name=" ++ name)
    , marker =
        previousMarker
            |> Marker.removeOne
            |> Marker.concat
                (List.map
                    (itemProperties
                        >> (\props -> props.name ++ "/" ++ props.id)
                        >> String.append usedPath
                        >> Marker.Directory
                    )
                    directories
                )
            |> (case nextPageToken of
                    Just token ->
                        { directory = usedDirectory, token = token }
                            |> Marker.Param
                            |> List.singleton
                            |> Marker.concat

                    Nothing ->
                        identity
               )
    }


itemDecoder : Decoder Item
itemDecoder =
    map4
        (\id name mime _ ->
            case mime of
                "application/vnd.google-apps.folder" ->
                    Directory { id = id, name = name }

                _ ->
                    File { id = id, name = name }
        )
        (field "id" string)
        (field "name" string)
        (field "mimeType" string)
        (andThen
            (\b ->
                if b then
                    fail "Exclude deleted files"

                else
                    succeed b
            )
            (field "trashed" bool)
        )


itemProperties : Item -> Properties
itemProperties item =
    case item of
        Directory props ->
            props

        File props ->
            props



-- ERROR


parseErrorResponse : String -> Maybe String
parseErrorResponse response =
    response
        |> decodeString (at [ "error", "message" ] string)
        |> Result.toMaybe
