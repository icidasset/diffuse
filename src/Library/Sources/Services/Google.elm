module Sources.Services.Google exposing (authorizationSourceData, authorizationUrl, defaultClientId, defaults, initialData, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties)

{-| Google Drive Service.
-}

import Base64
import Dict
import Dict.Ext as Dict
import Http
import Json.Decode
import Json.Encode
import Regex
import Sources exposing (Property, SourceData)
import Sources.Pick
import Sources.Processing exposing (..)
import Sources.Services.Google.Parser as Parser
import Time
import Url
import Url.Builder as Url



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
    [ { key = "authCode"
      , label = "Auth Code"
      , placeholder = "..."
      , password = True
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
    , { key = "folderId"
      , label = "Folder Id (Optional)"
      , placeholder = defaults.folderId
      , password = False
      }
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
    , ( "redirect_uri", origin ++ "/sources/new/google" )
    , ( "response_type", "code" )
    , ( "scope", "https://www.googleapis.com/auth/drive.readonly" )
    , ( "state", state )
    ]
        |> List.map (\( a, b ) -> Url.string a b)
        |> Url.toQuery
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
prepare origin srcData _ toMsg =
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
                |> List.map (\( a, b ) -> Url.string a b)
                |> Url.toQuery

        url =
            "https://www.googleapis.com/oauth2/v4/token" ++ query
    in
    (Just << Http.post)
        { url = url
        , body = Http.emptyBody
        , expect = Http.expectString toMsg
        }



-- TREE


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker currentTime toMsg =
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
                |> List.map (\( a, b ) -> Url.string a b)
                |> Url.toQuery
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = "https://www.googleapis.com/drive/v3/files" ++ params
        , body = Http.emptyBody
        , expect = Http.expectString toMsg
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


parseErrorResponse : String -> String
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
makeTrackUrl currentTime srcData method fileId =
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
