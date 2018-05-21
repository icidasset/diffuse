module Sources.Services.Dropbox exposing (..)

{-| Dropbox Service.
-}

import Base64
import Date exposing (Date)
import Dict
import Dict.Ext as Dict
import Http
import Json.Decode
import Json.Encode
import Regex
import Sources.Pick
import Sources.Processing.Types exposing (..)
import Sources.Services.Dropbox.Parser as Parser
import Sources.Services.Utils exposing (cleanPath, noPrep)
import Sources.Types exposing (SourceData)
import Time
import Utils exposing (encodeUri, makeQueryParam)


-- Properties
-- 📟


defaults =
    { appKey = "kwsydtrzban41zr"
    , directoryPath = "/"
    , name = "Music from Dropbox"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "accessToken", "Access Token", "...", True )
    , ( "appKey", "App key", defaults.appKey, False )
    , ( "directoryPath", "Directory", defaults.directoryPath, False )
    , ( "name", "Label", defaults.name, False )
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "accessToken", "" )
        , ( "appKey", defaults.appKey )
        , ( "directoryPath", defaults.directoryPath )
        , ( "name", defaults.name )
        ]



-- Authorization Procedure


{-| Authorization url.
-}
authorizationUrl : String -> SourceData -> String
authorizationUrl origin sourceData =
    let
        encodeData data =
            data
                |> Dict.toList
                |> List.map (Tuple.mapSecond Json.Encode.string)
                |> Json.Encode.object

        state =
            sourceData
                |> encodeData
                |> Json.Encode.encode 0
                |> Base64.encode
    in
        [ ( "response_type", "token" )
        , ( "client_id", Dict.fetch "appKey" "unknown" sourceData )
        , ( "redirect_uri", origin ++ "/sources/new/dropbox" )
        , ( "state", state )
        ]
            |> List.map makeQueryParam
            |> String.join "&"
            |> String.append "https://www.dropbox.com/oauth2/authorize?"


{-| Authorization source data.
-}
authorizationSourceData : String -> SourceData
authorizationSourceData hash =
    let
        mapFn item =
            case String.split "=" item of
                [ a, b ] ->
                    ( a, b )

                _ ->
                    ( "in", "valid" )

        hashDict =
            hash
                |> String.split "&"
                |> List.map mapFn
                |> Dict.fromList
    in
        hashDict
            |> Dict.get "state"
            |> Maybe.andThen Http.decodeUri
            |> Maybe.andThen (Base64.decode >> Result.toMaybe)
            |> Maybe.withDefault "{}"
            |> Json.Decode.decodeString (Json.Decode.dict Json.Decode.string)
            |> Result.withDefault Dict.empty
            |> Dict.unionF initialData
            |> Dict.update "accessToken" (always <| Dict.get "access_token" hashDict)



-- Preparation


prepare : String -> SourceData -> Marker -> Maybe (Http.Request String)
prepare _ _ _ =
    Nothing



-- Tree


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> Date -> Http.Request String
makeTree srcData marker currentDate =
    let
        accessToken =
            Dict.fetch "accessToken" "" srcData

        body =
            (case marker of
                TheBeginning ->
                    [ ( "limit", Json.Encode.int 2000 )
                    , ( "path", Json.Encode.string (getProperDirectoryPath srcData) )
                    , ( "recursive", Json.Encode.bool True )
                    ]

                InProgress cursor ->
                    [ ( "cursor", Json.Encode.string cursor )
                    ]

                TheEnd ->
                    []
            )
                |> Json.Encode.object
                |> Http.jsonBody

        url =
            case marker of
                TheBeginning ->
                    "https://api.dropboxapi.com/2/files/list_folder"

                InProgress _ ->
                    "https://api.dropboxapi.com/2/files/list_folder/continue"

                TheEnd ->
                    ""
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
            , url = url
            , body = body
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = False
            }


getProperDirectoryPath : SourceData -> String
getProperDirectoryPath srcData =
    let
        path =
            srcData
                |> Dict.get "directoryPath"
                |> Maybe.withDefault defaults.directoryPath
                |> cleanPath
    in
        if path == "" then
            ""
        else
            "/" ++ path


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    noPrep


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse =
    Parser.parseTreeResponse


parseErrorResponse : String -> String
parseErrorResponse =
    Parser.parseErrorResponse



-- Post


{-| Post process the tree results.

!!! Make sure we only use music files that we can use.

-}
postProcessTree : List String -> List String
postProcessTree =
    Sources.Pick.selectMusicFiles



-- Track URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method pathToFile =
    "dropbox://" ++ Dict.fetch "accessToken" "" srcData ++ "@" ++ pathToFile
