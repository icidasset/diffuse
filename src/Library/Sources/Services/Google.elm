module Sources.Services.Google exposing (authorizationSourceData, authorizationUrl, defaultClientId, defaults, initialData, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties)

{-| Google Drive Service.
-}

import Base64
import Common
import Conditional exposing (..)
import Dict
import Dict.Ext as Dict
import Http
import Json.Decode
import Json.Encode
import Sources exposing (Property, SourceData)
import Sources.Processing exposing (..)
import Sources.Services.Google.Marker as Marker
import Sources.Services.Google.Parser as Parser
import String.Path
import Time
import Url



-- PROPERTIES
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
properties : List Property
properties =
    [ { key = "folderId"
      , label = "Folder Id (Optional)"
      , placeholder = defaults.folderId
      , password = False
      }
    , { key = "clientId"
      , label = "Client Id (Google Console)"
      , placeholder = defaults.clientId
      , password = False
      }
    , { key = "clientSecret"
      , label = "Client Secret (Google Console)"
      , placeholder = defaults.clientSecret
      , password = False
      }
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "clientId", defaults.clientId )
        , ( "clientSecret", defaults.clientSecret )
        , ( "folderId", defaults.folderId )
        , ( "name", defaults.name )
        ]



-- AUTHORIZATION


{-| Authorization url.
-}
authorizationUrl : SourceData -> String -> String
authorizationUrl sourceData origin =
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
    , ( "redirect_uri", origin ++ "?path=sources/new/google" )
    , ( "response_type", "code" )
    , ( "scope", "https://www.googleapis.com/auth/drive.readonly" )
    , ( "state", state )
    ]
        |> Common.queryString
        |> String.append "https://accounts.google.com/o/oauth2/v2/auth"


{-| Authorization source data.
-}
authorizationSourceData : { codeOrToken : Maybe String, state : Maybe String } -> SourceData
authorizationSourceData args =
    args.state
        |> Maybe.andThen (Base64.decode >> Result.toMaybe)
        |> Maybe.withDefault "{}"
        |> Json.Decode.decodeString (Json.Decode.dict Json.Decode.string)
        |> Result.withDefault Dict.empty
        |> Dict.unionFlipped initialData
        |> Dict.update "authCode" (\_ -> args.codeOrToken)



-- PREPARATION


{-| Before processing we need to prepare the source.
In this case this means that we will refresh the `access_token`.
Or if we don't have an access token yet, get one.
-}
prepare : String -> SourceData -> Marker -> (Result Http.Error String -> msg) -> Maybe (Cmd msg)
prepare origin srcData _ resultMsg =
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
                    , ( "redirect_uri", origin ++ "?path=sources/new/google" )
                    ]

                -- Refresh access token
                Nothing ->
                    [ ( "client_id", Dict.fetch "clientId" "" srcData )
                    , ( "client_secret", Dict.fetch "clientSecret" "" srcData )
                    , ( "refresh_token", Dict.fetch "refreshToken" "" srcData )
                    , ( "grant_type", "refresh_token" )
                    ]

        query =
            Common.queryString queryParams

        url =
            "https://www.googleapis.com/oauth2/v4/token" ++ query
    in
    (Just << Http.post)
        { url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse resultMsg Common.translateHttpResponse
        }



-- TREE


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker currentTime resultMsg =
    let
        accessToken =
            Dict.fetch "accessToken" "" srcData

        folderId =
            Dict.fetch "folderId" "" srcData

        parentId =
            marker
                |> Marker.takeOne
                |> Maybe.map Marker.itemDirectory
                |> Maybe.andThen (\dir -> ifThenElse (String.isEmpty dir) Nothing <| Just dir)
                |> Maybe.withDefault folderId
                |> String.Path.file

        query =
            case parentId of
                "" ->
                    [ "mimeType contains 'audio/'" ]

                pid ->
                    [ "(mimeType contains 'audio/'"
                    , "or mimeType = 'application/vnd.google-apps.folder')"
                    , "and ('" ++ pid ++ "' in parents)"
                    ]

        paramsBase =
            [ ( "fields"
              , String.join ", "
                    [ "nextPageToken"
                    , "files/id"
                    , "files/mimeType"
                    , "files/name"
                    , "files/trashed"
                    ]
              )
            , ( "pageSize", "1000" )
            , ( "q", String.concat query )
            , ( "spaces", "drive" )
            ]

        queryString =
            (case Marker.takeOne marker of
                Just (Marker.Param { token }) ->
                    [ ( "pageToken", token ) ]

                _ ->
                    []
            )
                |> List.append paramsBase
                |> Common.queryString
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://www.googleapis.com/drive/v3/files" ++ queryString
        , body = Http.emptyBody
        , expect = Http.expectStringResponse resultMsg Common.translateHttpResponse
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    Parser.parsePreparationResponse


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse =
    Parser.parseTreeResponse


parseErrorResponse : String -> Maybe String
parseErrorResponse =
    Parser.parseErrorResponse



-- POST


{-| Post process the tree results.

!!! Make sure we only use music files that we can use.

-}
postProcessTree : List String -> List String
postProcessTree =
    identity



-- TRACK URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentTime srcData method path =
    let
        file =
            String.Path.file path

        fileId =
            file
                |> String.split "?"
                |> List.head
                |> Maybe.withDefault file

        accessToken =
            Dict.fetch "accessToken" "" srcData
    in
    String.concat
        [ "https://www.googleapis.com/drive/v3/files/"
        , Url.percentEncode fileId
        , "?alt=media&access_token="
        , Url.percentEncode accessToken
        ]
