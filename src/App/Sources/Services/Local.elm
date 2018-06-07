module Sources.Services.Local exposing (..)

{-| Local Service.

For those local files out there.

-}

import Date exposing (Date)
import Dict
import Dict.Ext as Dict
import Http
import Json.Decode
import Json.Encode
import Slave.Events exposing (..)
import Sources.Pick
import Sources.Processing.Types exposing (..)
import Sources.Services.Utils exposing (noPrep)
import Sources.Types exposing (SourceData)
import String.Ext as String
import Time
import Utils exposing (encodeUri)


electronServerUrl : String
electronServerUrl =
    "http://127.0.0.1:44999"



-- Properties
-- 📟


defaults =
    { localPath = "~/Music"
    , name = "Local Music"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "localPath", "Directory", defaults.localPath, False )
    , ( "name", "Label", defaults.name, False )
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "localPath", defaults.localPath )
        , ( "name", defaults.name )
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
        dir =
            Dict.fetch "localPath" defaults.localPath srcData

        url =
            electronServerUrl ++ "/local/tree?path=" ++ encodeUri dir
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
parseTreeResponse response _ =
    let
        paths =
            response
                |> Json.Decode.decodeString (Json.Decode.list Json.Decode.string)
                |> Result.withDefault []
    in
        { filePaths = paths
        , marker = TheEnd
        }


parseErrorResponse : String -> String
parseErrorResponse =
    identity



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

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData method pathToFile =
    let
        dir =
            srcData
                |> Dict.fetch "localPath" defaults.localPath
                |> String.chop "/"
    in
        electronServerUrl ++ "/local/file?path=" ++ encodeUri (dir ++ "/" ++ pathToFile)
