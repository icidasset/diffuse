module Sources.Services.AzureBlob exposing (defaults, initialData, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties)

{-| Microsoft Azure Blob Service.

Resources:

  - <https://docs.microsoft.com/en-us/rest/api/storageservices/blob-service-rest-api>

-}

import Common
import Dict
import Http
import Sources exposing (Property, SourceData)
import Sources.Pick
import Sources.Processing exposing (..)
import Sources.Services.Azure.Authorization exposing (..)
import Sources.Services.Azure.BlobParser as Parser
import Sources.Services.Common exposing (cleanPath, noPrep)
import Time



-- PROPERTIES
-- 📟


defaults =
    { name = "Music from Azure Blob Storage"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "accountName"
      , label = "Account name"
      , placeholder = "myaccount"
      , password = False
      }
    , { key = "accountKey"
      , label = "Account key"
      , placeholder = "MXFPDkaN4KBT"
      , password = True
      }
    , { key = "container"
      , label = "Container"
      , placeholder = "music"
      , password = False
      }
    , { key = "directoryPath"
      , label = "Directory (aka. Prefix, Optional)"
      , placeholder = "/"
      , password = False
      }
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "accountName", "" )
        , ( "accountKey", "" )
        , ( "container", "" )
        , ( "directoryPath", "" )
        , ( "name", defaults.name )
        ]



-- PREPARATION


prepare : String -> SourceData -> Marker -> (Result Http.Error String -> msg) -> Maybe (Cmd msg)
prepare _ _ _ _ =
    Nothing



-- TREE


{-| Create a directory tree.

List all the tracks in the container.
Or a specific directory in the container.

-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker currentTime resultMsg =
    let
        directoryPath =
            srcData
                |> Dict.get "directoryPath"
                |> Maybe.withDefault ""
                |> cleanPath

        baseParams =
            [ ( "maxresults", "1000" ) ]

        params =
            case marker of
                InProgress s ->
                    [ ( "marker", s ) ]

                _ ->
                    []

        url =
            presignedUrl Blob List Get 1 currentTime srcData directoryPath (baseParams ++ params)
    in
    Http.get
        { url = url
        , expect = Http.expectStringResponse resultMsg Common.translateHttpResponse
        }


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
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
    Sources.Pick.selectMusicFiles



-- TRACK URL


{-| Create a public url for a file.

We need this to play the track.
(!) Creates a presigned url that's valid for 48 hours

-}
makeTrackUrl : Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentTime srcData method pathToFile =
    presignedUrl Blob Read Get 48 currentTime srcData pathToFile []
