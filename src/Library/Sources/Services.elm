module Sources.Services exposing (initialData, keyToType, labels, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties, typeToKey)

{-| Service functions used in other modules.
-}

import Http
import Sources exposing (..)
import Sources.Processing exposing (..)
import Sources.Services.AmazonS3 as AmazonS3
import Sources.Services.Dropbox as Dropbox
import Sources.Services.Google as Google
import Sources.Services.Ipfs as Ipfs
import Time



-- FUNCTIONS


initialData : Service -> SourceData
initialData service =
    case service of
        AmazonS3 ->
            AmazonS3.initialData

        Dropbox ->
            Dropbox.initialData

        Ipfs ->
            Ipfs.initialData

        Google ->
            Google.initialData


makeTrackUrl : Service -> Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl service =
    case service of
        AmazonS3 ->
            AmazonS3.makeTrackUrl

        Dropbox ->
            Dropbox.makeTrackUrl

        Ipfs ->
            Ipfs.makeTrackUrl

        Google ->
            Google.makeTrackUrl


makeTree :
    Service
    -> SourceData
    -> Marker
    -> Time.Posix
    -> (Result Http.Error String -> msg)
    -> Cmd msg
makeTree service =
    case service of
        AmazonS3 ->
            AmazonS3.makeTree

        Dropbox ->
            Dropbox.makeTree

        Ipfs ->
            Ipfs.makeTree

        Google ->
            Google.makeTree


parseErrorResponse : Service -> String -> String
parseErrorResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseErrorResponse

        Dropbox ->
            Dropbox.parseErrorResponse

        Ipfs ->
            Ipfs.parseErrorResponse

        Google ->
            Google.parseErrorResponse


parsePreparationResponse : Service -> String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parsePreparationResponse

        Dropbox ->
            Dropbox.parsePreparationResponse

        Ipfs ->
            Ipfs.parsePreparationResponse

        Google ->
            Google.parsePreparationResponse


parseTreeResponse : Service -> String -> Marker -> TreeAnswer Marker
parseTreeResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseTreeResponse

        Dropbox ->
            Dropbox.parseTreeResponse

        Ipfs ->
            Ipfs.parseTreeResponse

        Google ->
            Google.parseTreeResponse


postProcessTree : Service -> List String -> List String
postProcessTree service =
    case service of
        AmazonS3 ->
            AmazonS3.postProcessTree

        Dropbox ->
            Dropbox.postProcessTree

        Ipfs ->
            Ipfs.postProcessTree

        Google ->
            Google.postProcessTree


prepare :
    Service
    -> String
    -> SourceData
    -> Marker
    -> (Result Http.Error String -> msg)
    -> Maybe (Cmd msg)
prepare service =
    case service of
        AmazonS3 ->
            AmazonS3.prepare

        Dropbox ->
            Dropbox.prepare

        Ipfs ->
            Ipfs.prepare

        Google ->
            Google.prepare


properties : Service -> List Property
properties service =
    case service of
        AmazonS3 ->
            AmazonS3.properties

        Dropbox ->
            Dropbox.properties

        Ipfs ->
            Ipfs.properties

        Google ->
            Google.properties



-- KEYS & LABELS


keyToType : String -> Maybe Service
keyToType str =
    case str of
        "AmazonS3" ->
            Just AmazonS3

        "Dropbox" ->
            Just Dropbox

        "Ipfs" ->
            Just Ipfs

        "Google" ->
            Just Google

        _ ->
            Nothing


typeToKey : Service -> String
typeToKey service =
    case service of
        AmazonS3 ->
            "AmazonS3"

        Dropbox ->
            "Dropbox"

        Ipfs ->
            "Ipfs"

        Google ->
            "Google"


{-| Service labels.
Maps a service key to a label.
-}
labels : List ( String, String )
labels =
    [ ( typeToKey AmazonS3, "Amazon S3" )
    , ( typeToKey Dropbox, "Dropbox" )
    , ( typeToKey Google, "Google Drive" )
    , ( typeToKey Ipfs, "IPFS" )
    ]
