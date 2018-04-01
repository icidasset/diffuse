module Sources.Services.AzureBlob exposing (..)

{-| Microsoft Azure Blob Service.

Resources:

  - <https://docs.microsoft.com/en-us/rest/api/storageservices/blob-service-rest-api>

-}

import Date exposing (Date)
import Dict
import Http
import Sources.Pick
import Sources.Services.Azure.Authorization exposing (..)
import Sources.Services.Azure.BlobParser as Parser
import Sources.Services.Utils exposing (cleanPath)
import Sources.Processing.Types exposing (..)
import Sources.Types exposing (SourceData)
import Time


-- Properties
-- 📟


defaults =
    { directoryPath = "/"
    , name = "Music from Azure Blob Storage"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "accountName", "Account name", "myaccount", False )
    , ( "accountKey", "Account key", "MXFPDkaN4KBT", True )
    , ( "container", "Container", "music", False )
    , ( "directoryPath", "Directory (aka. Prefix)", defaults.directoryPath, False )
    , ( "name", "Label", defaults.name, False )
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "accountName", "" )
        , ( "accountKey", "" )
        , ( "container", "" )
        , ( "directoryPath", defaults.directoryPath )
        , ( "name", defaults.name )
        ]



-- Track URL


{-| Create a public url for a file.

We need this to play the track.
(!) Creates a presigned url that's valid for 24 hours

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method pathToFile =
    presignedUrl Blob Read Get 24 currentDate srcData pathToFile []



-- Tree


{-| Create a directory tree.

List all the tracks in the container.
Or a specific directory in the container.

-}
makeTree : SourceData -> Marker -> (TreeStepResult -> msg) -> Date -> Cmd msg
makeTree srcData marker msg currentDate =
    let
        directoryPath =
            srcData
                |> Dict.get "directoryPath"
                |> Maybe.withDefault defaults.directoryPath
                |> cleanPath

        initialParams =
            []

        prefix =
            if String.length directoryPath > 0 then
                [ ( "prefix", directoryPath ) ]
            else
                []

        continuation =
            case marker of
                InProgress s ->
                    [ ( "marker", s ) ]

                _ ->
                    []

        params =
            initialParams ++ prefix ++ continuation

        url =
            presignedUrl Blob List Get 1 currentDate srcData "" params
    in
        url
            |> Http.getString
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
