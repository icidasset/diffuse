module Sources.Services.WebDav exposing (..)

{-| WebDAV Service.

Resources:

  - <https://en.wikipedia.org/wiki/WebDAV>

-}

import Base64
import Date exposing (Date)
import Dict
import Dict.Ext as Dict
import Erl
import Http
import Sources.Services.Local exposing (electronServerUrl)
import Sources.Services.WebDav.Marker as Marker
import Sources.Services.WebDav.Parser as Parser
import Sources.Services.Utils exposing (cleanPath, noPrep)
import Sources.Processing.Types exposing (..)
import Sources.Types exposing (SourceData)


-- Properties
-- 📟


defaults =
    { name = "Music from WebDAV"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "url", "Host URL", "https://demo.nextcloud.com", False )
    , ( "username", "Username", "", False )
    , ( "password", "Password", "", True )
    , ( "directoryPath", "Directory", "/icidasset/remote.php/webdav/", False )
    , ( "name", "Label", defaults.name, False )
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "directoryPath", "" )
        , ( "url", "" )
        , ( "username", "" )
        , ( "password", "" )
        , ( "name", defaults.name )
        ]



-- Preparation


prepare : String -> SourceData -> Marker -> Maybe (Http.Request String)
prepare _ _ _ =
    Nothing



-- Tree


{-| Create a directory tree.
-}
makeTree : SourceData -> Marker -> Date -> (Result Http.Error String -> Msg) -> Cmd Msg
makeTree srcData marker currentDate resultMsg =
    let
        username =
            Dict.fetch "username" "" srcData

        password =
            Dict.fetch "password" "" srcData

        authToken =
            Base64.encode (username ++ ":" ++ password)

        host =
            Dict.fetch "url" "" srcData

        inProgress =
            case marker of
                InProgress _ ->
                    True

                _ ->
                    False

        directory =
            case marker of
                InProgress _ ->
                    marker
                        |> Marker.takeOne
                        |> Maybe.withDefault ""

                _ ->
                    srcData
                        |> Dict.get "directoryPath"
                        |> Maybe.withDefault ""

        localUrl =
            String.concat
                [ electronServerUrl
                , "/webdav/tree?url="
                , { host = host, directory = directory, removeHostPath = inProgress }
                    |> properUrl
                    |> Http.encodeUri
                , "&auth="
                , (username ++ ":" ++ password)
                    |> Base64.encode
                    |> String.append "Basic "
                ]
    in
        { method = "PROPFIND"
        , headers = []
        , url = localUrl
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.request
            |> Http.send resultMsg


properUrl : { host : String, directory : String, removeHostPath : Bool } -> String
properUrl { host, directory, removeHostPath } =
    let
        base =
            Erl.parse host
    in
        String.concat
            [ { base | path = [], query = [], hash = "" }
                |> Erl.toString
                |> cleanPath
            , if removeHostPath then
                ""
              else
                base
                    |> Erl.toAbsoluteString
                    |> cleanPath
            , cleanPath directory
            ]


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
    identity



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
makeTrackUrl _ srcData httpMethod filePath =
    let
        host =
            Dict.fetch "url" "" srcData

        username =
            Dict.fetch "username" "" srcData

        password =
            Dict.fetch "password" "" srcData
    in
        String.concat
            [ electronServerUrl
            , "/webdav/file?url="
            , { host = host, directory = filePath, removeHostPath = True }
                |> properUrl
                |> String.dropRight 1
                |> Http.encodeUri
            , "&auth="
            , (username ++ ":" ++ password)
                |> Base64.encode
                |> String.append "Basic "
            ]
