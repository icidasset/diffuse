module Sources.Services.Google exposing (..)

{-| Google Drive Service.
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
import Sources.Services.Google.Parser as Parser
import Sources.Types exposing (SourceData)
import Time
import Utils exposing (encodeUri, makeQueryParam)


-- Properties
-- 📟


defaults =
    { clientId = defaultClientId
    , clientSecret = "uHBInBeGnA38FOlpLTEyPlUv"
    , folderId = ""
    , name = "Music from Google Drive"
    }


defaultClientId : String
defaultClientId =
    String.concat
        [ "720114869239-74amkqeila5ursobjqvo9c263u1cllhu"
        , ".apps.googleusercontent.com"
        ]


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "authCode", "Auth Code", "...", True )
    , ( "clientId", "Client Id (Google Console)", defaults.clientId, False )
    , ( "clientSecret", "Client Secret (Google Console)", defaults.clientSecret, False )
    , ( "folderId", "Folder Id (Optional)", defaults.folderId, False )
    , ( "name", "Label", defaults.name, False )
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "authCode", "" )
        , ( "clientId", defaults.clientId )
        , ( "clientSecret", defaults.clientSecret )
        , ( "folderId", defaults.folderId )
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
        [ ( "access_type", "offline" )
        , ( "client_id", Dict.fetch "clientId" "unknown" sourceData )
        , ( "prompt", "consent" )
        , ( "redirect_uri", origin ++ "/sources/new/google" )
        , ( "response_type", "code" )
        , ( "scope", "https://www.googleapis.com/auth/drive.readonly" )
        , ( "state", state )
        ]
            |> List.map makeQueryParam
            |> String.join "&"
            |> String.append "https://accounts.google.com/o/oauth2/v2/auth?"


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
            |> Dict.update "authCode" (always <| Dict.get "code" hashDict)



-- Preparation


{-| Before processing we need to prepare the source.
In this case this means that we will refresh the `access_token`.
Or if we don't have an access token yet, get one.
-}
prepare : String -> SourceData -> Marker -> Maybe (Http.Request String)
prepare origin srcData _ =
    let
        maybeCode =
            Dict.get "authCode" srcData

        queryParams =
            case maybeCode of
                -- Exchange authorization code for access token & request token
                Just authCode ->
                    [ ( "client_id", Dict.fetch "clientId" "" srcData )
                    , ( "client_secret", Dict.fetch "clientSecret" "" srcData )
                    , ( "code", Dict.fetch "authCode" "" srcData )
                    , ( "grant_type", "authorization_code" )
                    , ( "redirect_uri", origin ++ "/sources/new/google" )
                    ]

                -- Refresh access token
                Nothing ->
                    [ ( "client_id", Dict.fetch "clientId" "" srcData )
                    , ( "client_secret", Dict.fetch "clientSecret" "" srcData )
                    , ( "refresh_token", Dict.fetch "refreshToken" "" srcData )
                    , ( "grant_type", "refresh_token" )
                    ]

        query =
            queryParams
                |> List.map makeQueryParam
                |> String.join "&"

        url =
            "https://www.googleapis.com/oauth2/v4/token?" ++ query
    in
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.request
            |> Just



-- Tree


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> Date -> (Result Http.Error String -> Msg) -> Cmd Msg
makeTree srcData marker currentDate resultMsg =
    let
        accessToken =
            Dict.fetch "accessToken" "" srcData

        folderId =
            Dict.fetch "folderId" "" srcData

        queryBase =
            [ "mimeType contains 'audio/'" ]

        query =
            case folderId of
                "" ->
                    queryBase

                fid ->
                    queryBase ++ [ "'" ++ fid ++ "' in parents" ]

        paramsBase =
            [ ( "pageSize", "1000" )
            , ( "q", String.join " and " query )
            , ( "spaces", "drive" )
            ]

        params =
            (case marker of
                InProgress cursor ->
                    [ ( "pageToken", cursor )
                    ]

                _ ->
                    []
            )
                |> List.append paramsBase
                |> List.map makeQueryParam
                |> String.join "&"
    in
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://www.googleapis.com/drive/v3/files?" ++ params
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.request
            |> Http.send resultMsg


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    Parser.parsePreparationResponse


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
    identity



-- Track URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method fileId =
    let
        accessToken =
            Dict.fetch "accessToken" "" srcData
    in
        String.concat
            [ "https://www.googleapis.com/drive/v3/files/"
            , fileId
            , "?alt=media&access_token="
            , accessToken
            ]
