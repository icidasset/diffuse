module Sources.Services.AmazonS3 exposing (..)

{-| Amazon S3 Service.

Resources:

  - <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html>

-}

import Date exposing (Date)
import Dict
import Http
import Regex
import Sources.Pick
import Sources.Services.AmazonS3.Parser as Parser
import Sources.Services.AmazonS3.Presign exposing (..)
import Sources.Services.Utils exposing (cleanPath, noPrep)
import Sources.Processing.Types exposing (..)
import Sources.Types exposing (SourceData)
import Time


-- Properties
-- 📟


defaults =
    { directoryPath = "/"
    , name = "Music from Amazon S3"
    , region = "eu-west-1"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "accessKey", "Access key", "Fv6EWfLfCcMo", True )
    , ( "secretKey", "Secret key", "qeNcqiMpgqC8", True )
    , ( "bucketName", "Bucket name", "music", False )
    , ( "region", "Region", defaults.region, False )
    , ( "directoryPath", "Directory", defaults.directoryPath, False )
    , ( "name", "Label", defaults.name, False )
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "accessKey", "" )
        , ( "bucketName", "" )
        , ( "directoryPath", defaults.directoryPath )
        , ( "name", defaults.name )
        , ( "region", defaults.region )
        , ( "secretKey", "" )
        ]



-- Preparation


prepare : String -> SourceData -> Marker -> Maybe (Http.Request String)
prepare _ _ _ =
    Nothing



-- Tree


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> Date -> (Result Http.Error String -> Msg) -> Cmd Msg
makeTree srcData marker currentDate resultMsg =
    let
        directoryPath =
            srcData
                |> Dict.get "directoryPath"
                |> Maybe.withDefault defaults.directoryPath
                |> cleanPath

        initialParams =
            [ ( "list-type", "2" )
            , ( "max-keys", "1000" )
            ]

        prefix =
            if String.length directoryPath > 0 then
                [ ( "prefix", directoryPath ) ]
            else
                []

        continuation =
            case marker of
                InProgress s ->
                    [ ( "continuation-token", s ) ]

                _ ->
                    []

        params =
            initialParams ++ prefix ++ continuation

        url =
            presignedUrl Get (Time.second * 60 * 5) params currentDate srcData "/"
    in
        url
            |> Http.getString
            |> Http.send resultMsg


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
(!) Creates a presigned url that's valid for 48 hours

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method pathToFile =
    presignedUrl method (Time.second * 172800) [] currentDate srcData pathToFile
