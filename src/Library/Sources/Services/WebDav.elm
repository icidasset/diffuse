module Sources.Services.WebDav exposing (defaults, initialData, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties)

{-| IPFS Service.

Resources:

  - <https://ipfs.io/docs/api/>

-}

import Base64
import Common
import Dict
import Dict.Ext as Dict
import Http
import Sources exposing (Property, SourceData)
import Sources.Pick exposing (selectMusicFiles)
import Sources.Processing exposing (..)
import Sources.Services.Common exposing (noPrep)
import Sources.Services.WebDav.Marker as Marker
import Sources.Services.WebDav.Parser as Parser
import String.Ext as String
import Time
import Url



-- PROPERTIES
-- 📟


defaults =
    { name = "Music from WebDAV"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "url"
      , label = "Host URL"
      , placeholder = "https://demo.nextcloud.com"
      , password = False
      }
    , { key = "directoryPath"
      , label = "Directory (Optional)"
      , placeholder = "/icidasset/remote.php/webdav/"
      , password = False
      }
    , { key = "username"
      , label = "Username (Optional)"
      , placeholder = ""
      , password = False
      }
    , { key = "password"
      , label = "Password (Optional)"
      , placeholder = ""
      , password = True
      }
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



-- PREPARATION


prepare : String -> SourceData -> Marker -> (Result Http.Error String -> msg) -> Maybe (Cmd msg)
prepare _ _ _ _ =
    Nothing



-- TREE


{-| Create a directory tree.
-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker _ resultMsg =
    let
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

        username =
            Dict.fetch "username" "" srcData

        password =
            Dict.fetch "password" "" srcData

        auth =
            "Basic " ++ Base64.encode (username ++ ":" ++ password)
    in
    Http.request
        { method = "PROPFIND"
        , headers = [ Http.header "Authorization" auth, Http.header "Depth" "1" ]
        , url = url { addAuth = False } srcData directory
        , body = Http.emptyBody
        , expect = Http.expectStringResponse resultMsg Common.translateHttpResponse
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> Time.Posix -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    noPrep


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
    selectMusicFiles



-- TRACK URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Time.Posix -> String -> SourceData -> HttpMethod -> String -> String
makeTrackUrl _ _ srcData _ filePath =
    url { addAuth = True } srcData filePath



-- COMMON


url : { addAuth : Bool } -> SourceData -> String -> String
url { addAuth } srcData path =
    let
        host =
            String.chopEnd "/" (Dict.fetch "url" "" srcData)

        username =
            Dict.fetch "username" "" srcData

        password =
            Dict.fetch "password" "" srcData

        authPrefix =
            case ( username, password ) of
                ( "", "" ) ->
                    ""

                ( u, p ) ->
                    u ++ ":" ++ p

        authBit =
            if addAuth && String.length authPrefix > 0 then
                "?basic_auth=" ++ Url.percentEncode (Base64.encode authPrefix)

            else
                ""

        encodedPath =
            path
                |> String.chopStart "/"
                |> String.split "/"
                |> List.map Url.percentEncode
                |> String.join "/"
    in
    host ++ "/" ++ encodedPath ++ authBit
