module Sources.Services exposing (initialData, keyToType, labels, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties, typeToKey)

{-| Service functions used in other modules.
-}

import Http
import Sources exposing (..)
import Sources.Processing exposing (..)
import Sources.Services.AmazonS3 as AmazonS3
import Sources.Services.AzureBlob as AzureBlob
import Sources.Services.AzureFile as AzureFile
import Sources.Services.Dropbox as Dropbox
import Sources.Services.Google as Google
import Sources.Services.Ipfs as Ipfs
import Sources.Services.WebDav as WebDav
import Time



-- FUNCTIONS


initialData : Service -> SourceData
initialData service =
    case service of
        AmazonS3 ->
            AmazonS3.initialData

        AzureBlob ->
            AzureBlob.initialData

        AzureFile ->
            AzureFile.initialData

        Dropbox ->
            Dropbox.initialData

        Google ->
            Google.initialData

        Ipfs ->
            Ipfs.initialData

        WebDav ->
            WebDav.initialData


makeTrackUrl : Service -> Time.Posix -> String -> SourceData -> HttpMethod -> String -> String
makeTrackUrl service =
    case service of
        AmazonS3 ->
            AmazonS3.makeTrackUrl

        AzureBlob ->
            AzureBlob.makeTrackUrl

        AzureFile ->
            AzureFile.makeTrackUrl

        Dropbox ->
            Dropbox.makeTrackUrl

        Google ->
            Google.makeTrackUrl

        Ipfs ->
            Ipfs.makeTrackUrl

        WebDav ->
            WebDav.makeTrackUrl


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

        AzureBlob ->
            AzureBlob.makeTree

        AzureFile ->
            AzureFile.makeTree

        Dropbox ->
            Dropbox.makeTree

        Google ->
            Google.makeTree

        Ipfs ->
            Ipfs.makeTree

        WebDav ->
            WebDav.makeTree


parseErrorResponse : Service -> String -> Maybe String
parseErrorResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseErrorResponse

        AzureBlob ->
            AzureBlob.parseErrorResponse

        AzureFile ->
            AzureFile.parseErrorResponse

        Dropbox ->
            Dropbox.parseErrorResponse

        Google ->
            Google.parseErrorResponse

        Ipfs ->
            Ipfs.parseErrorResponse

        WebDav ->
            WebDav.parseErrorResponse


parsePreparationResponse : Service -> String -> Time.Posix -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parsePreparationResponse

        AzureBlob ->
            AzureBlob.parsePreparationResponse

        AzureFile ->
            AzureFile.parsePreparationResponse

        Dropbox ->
            Dropbox.parsePreparationResponse

        Google ->
            Google.parsePreparationResponse

        Ipfs ->
            Ipfs.parsePreparationResponse

        WebDav ->
            WebDav.parsePreparationResponse


parseTreeResponse : Service -> String -> Marker -> TreeAnswer Marker
parseTreeResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseTreeResponse

        AzureBlob ->
            AzureBlob.parseTreeResponse

        AzureFile ->
            AzureFile.parseTreeResponse

        Dropbox ->
            Dropbox.parseTreeResponse

        Google ->
            Google.parseTreeResponse

        Ipfs ->
            Ipfs.parseTreeResponse

        WebDav ->
            WebDav.parseTreeResponse


postProcessTree : Service -> List String -> List String
postProcessTree service =
    case service of
        AmazonS3 ->
            AmazonS3.postProcessTree

        AzureBlob ->
            AzureBlob.postProcessTree

        AzureFile ->
            AzureFile.postProcessTree

        Dropbox ->
            Dropbox.postProcessTree

        Google ->
            Google.postProcessTree

        Ipfs ->
            Ipfs.postProcessTree

        WebDav ->
            WebDav.postProcessTree


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

        AzureBlob ->
            AzureBlob.prepare

        AzureFile ->
            AzureFile.prepare

        Dropbox ->
            Dropbox.prepare

        Google ->
            Google.prepare

        Ipfs ->
            Ipfs.prepare

        WebDav ->
            WebDav.prepare


properties : Service -> List Property
properties service =
    case service of
        AmazonS3 ->
            AmazonS3.properties

        AzureBlob ->
            AzureBlob.properties

        AzureFile ->
            AzureFile.properties

        Dropbox ->
            Dropbox.properties

        Google ->
            Google.properties

        Ipfs ->
            Ipfs.properties

        WebDav ->
            WebDav.properties



-- KEYS & LABELS


keyToType : String -> Maybe Service
keyToType str =
    case str of
        "AmazonS3" ->
            Just AmazonS3

        "AzureBlob" ->
            Just AzureBlob

        "AzureFile" ->
            Just AzureFile

        "Dropbox" ->
            Just Dropbox

        "Google" ->
            Just Google

        "Ipfs" ->
            Just Ipfs

        "WebDav" ->
            Just WebDav

        _ ->
            Nothing


typeToKey : Service -> String
typeToKey service =
    case service of
        AmazonS3 ->
            "AmazonS3"

        AzureBlob ->
            "AzureBlob"

        AzureFile ->
            "AzureFile"

        Dropbox ->
            "Dropbox"

        Google ->
            "Google"

        Ipfs ->
            "Ipfs"

        WebDav ->
            "WebDav"


{-| Service labels.
Maps a service key to a label.
-}
labels : List ( String, String )
labels =
    [ ( typeToKey AmazonS3, "Amazon S3" )
    , ( typeToKey AzureBlob, "Azure Blob Storage" )
    , ( typeToKey AzureFile, "Azure File Storage" )
    , ( typeToKey Dropbox, "Dropbox" )
    , ( typeToKey Ipfs, "IPFS" )
    , ( typeToKey WebDav, "WebDAV" )
    ]
