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


{-| Authorization url.
-}
authorizationUrl : SourceData -> String
authorizationUrl sourceData =
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
        , ( "redirect_uri", "http://localhost:5000/sources/new/dropbox" )
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



-- Track URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method pathToFile =
    "dropbox://" ++ Dict.fetch "accessToken" "" srcData ++ "@" ++ pathToFile



-- Tree


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> (TreeStepResult -> msg) -> Date -> Cmd msg
makeTree srcData marker msg currentDate =
    let
        directoryPath =
            srcData
                |> Dict.get "directoryPath"
                |> Maybe.withDefault defaults.directoryPath
                |> String.trim
                |> Regex.replace Regex.All (Regex.regex "(^\\/|\\/$)") (\_ -> "")
                |> (\d ->
                        if String.isEmpty d then
                            "/"
                        else
                            "/" ++ d ++ "/"
                   )

        accessToken =
            Dict.fetch "accessToken" "" srcData

        body =
            [ ( "path", Json.Encode.string directoryPath )
            , ( "recursive", Json.Encode.bool True )
            ]
                |> Json.Encode.object
                |> Http.jsonBody

        url =
            "https://api.dropboxapi.com/2/files/list_folder"
    in
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ accessToken) ]
        , url = url
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.request
            |> Http.send msg


{-| Re-export parser functions.
-}
parseTreeResponse : String -> Marker -> ParsedResponse Marker
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
