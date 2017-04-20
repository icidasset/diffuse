module Sources.Services.AmazonS3 exposing (..)

{-| Amazon S3 Service.

Resources:
- http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html

-}

import Date exposing (Date)
import Dict
import Http
import Regex
import Sources.Services.AmazonS3.Parser as Parser
import Sources.Services.AmazonS3.Presign exposing (..)
import Sources.Types exposing (..)
import Time


-- Properties
-- 📟


defaults =
    { directoryPath = "/"
    , name = "Amazon S3 source"
    , region = "eu-west-1"
    }


{-| The list of properties we need from the user.
    `(property, label, placeholder, isPassword)`
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



-- Track URL


{-| Create a public url for a file.
    We need this to play the track.
    (!) Creates a presigned url that's valid for 24 hours
-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method pathToFile =
    presignedUrl method (Time.second * 86400) [] currentDate srcData pathToFile



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
                            d
                        else
                            d ++ "/"
                   )

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
            |> Http.send msg


{-| Re-export parser functions.
-}
parseTreeResponse : String -> ParsedResponse Marker
parseTreeResponse =
    Parser.parseTreeResponse


parseErrorResponse : String -> String
parseErrorResponse =
    Parser.parseErrorResponse
