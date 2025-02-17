module Sources.Services.Ipfs exposing (..)

{-| IPFS Service.

Resources:

  - <https://ipfs.io/docs/api/>

-}

import Common exposing (boolFromString, boolToString)
import Conditional exposing (ifThenElse)
import Dict
import Dict.Ext as Dict
import Http
import Json.Decode as Json
import Sources exposing (Property, SourceData)
import Sources.Processing exposing (..)
import Sources.Services.Ipfs.Marker as Marker
import Sources.Services.Ipfs.Parser as Parser
import String.Ext as String
import Task
import Time
import Url



-- PROPERTIES
-- 📟


defaults =
    { gateway = ""
    , local = boolToString False
    , name = "Music from IPFS"
    , ipns = boolToString False
    }


defaultGateway =
    "https://ipfs.io"


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "directoryHash"
      , label = "Directory hash / DNSLink domain"
      , placeholder = "QmVLDAhCY3X9P2u"
      , password = False
      }
    , { key = "gateway"
      , label = "Gateway (Optional)"
      , placeholder = defaultGateway
      , password = False
      }
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "directoryHash", "" )
        , ( "gateway", defaults.gateway )
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
        gateway =
            extractGateway srcData

        r =
            root srcData

        path =
            case marker of
                InProgress _ ->
                    marker
                        |> Marker.takeOne
                        |> Maybe.map (\p -> r ++ "/" ++ p)
                        |> Maybe.withDefault ""

                _ ->
                    r
    in
    { method = "GET"
    , headers = [ Http.header "Accept" "application/vnd.ipld.dag-json" ]
    , url = gateway ++ path ++ "?format=dag-json"
    , body = Http.emptyBody
    , resolver = Http.stringResolver Common.translateHttpResponse
    , timeout = Just (60 * 5 * 1000) -- 5 minutes
    }
        |> Http.task
        |> Task.attempt resultMsg


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> Time.Posix -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    Parser.parseDnsLookup


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
makeTrackUrl : Time.Posix -> String -> SourceData -> HttpMethod -> String -> String
makeTrackUrl _ _ srcData _ path =
    extractGateway srcData ++ root srcData ++ "/" ++ encodedPath path



-- ⚗️


encodedPath : String -> String
encodedPath path =
    path
        |> String.split "/"
        |> List.map Url.percentEncode
        |> String.join "/"


extractGateway : SourceData -> String
extractGateway srcData =
    srcData
        |> Dict.get "gateway"
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\s ->
                case s of
                    "" ->
                        Nothing

                    _ ->
                        Just s
            )
        |> Maybe.map (String.chopEnd "/")
        |> Maybe.withDefault defaultGateway


isDnsLink : SourceData -> Bool
isDnsLink srcData =
    srcData
        |> Dict.get "directoryHash"
        |> Maybe.map pathIsDnsLink
        |> Maybe.withDefault False


pathIsDnsLink : String -> Bool
pathIsDnsLink =
    String.contains "."


root : SourceData -> String
root srcData =
    srcData
        |> Dict.get "directoryHash"
        |> Maybe.withDefault ""
        |> String.chopEnd "/"
        |> (if isDnsLink srcData then
                String.append "/ipns/"

            else
                String.append "/ipfs/"
           )
