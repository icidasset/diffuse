module Sources.Services exposing (..)

{-| Service functions used in other modules.
-}

import Date exposing (Date)
import Sources.Processing.Types exposing (..)
import Sources.Types exposing (..)


-- Services

import Sources.Services.AmazonS3 as AmazonS3
import Sources.Services.Ipfs as Ipfs
import Sources.Services.Local as Local


-- Functions implemented by services


initialData : Service -> SourceData
initialData service =
    case service of
        AmazonS3 ->
            AmazonS3.initialData

        Ipfs ->
            Ipfs.initialData

        Local ->
            Local.initialData


makeTrackUrl : Service -> Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl service =
    case service of
        AmazonS3 ->
            AmazonS3.makeTrackUrl

        Ipfs ->
            Ipfs.makeTrackUrl

        Local ->
            Local.makeTrackUrl


makeTree : Service -> SourceData -> Marker -> (TreeStepResult -> msg) -> Date -> Cmd msg
makeTree service =
    case service of
        AmazonS3 ->
            AmazonS3.makeTree

        Ipfs ->
            Ipfs.makeTree

        Local ->
            Local.makeTree


parseErrorResponse : Service -> String -> String
parseErrorResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseErrorResponse

        Ipfs ->
            Ipfs.parseErrorResponse

        Local ->
            Local.parseErrorResponse


parseTreeResponse : Service -> String -> Marker -> ParsedResponse Marker
parseTreeResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseTreeResponse

        Ipfs ->
            Ipfs.parseTreeResponse

        Local ->
            Local.parseTreeResponse


postProcessTree : Service -> List String -> List String
postProcessTree service =
    case service of
        AmazonS3 ->
            AmazonS3.postProcessTree

        Ipfs ->
            Ipfs.postProcessTree

        Local ->
            Local.postProcessTree


properties : Service -> List ( String, String, String, Bool )
properties service =
    case service of
        AmazonS3 ->
            AmazonS3.properties

        Ipfs ->
            Ipfs.properties

        Local ->
            Local.properties



-- Utility functions


makeSource : Service -> SourceData -> Source
makeSource service data =
    { id = "change_me_please"
    , data = data
    , directoryPlaylists = True
    , enabled = True
    , service = service
    }


keyToType : String -> Service
keyToType str =
    case str of
        "AmazonS3" ->
            AmazonS3

        "Ipfs" ->
            Ipfs

        "Local" ->
            Local

        _ ->
            Debug.crash "Invalid Service type string"


typeToKey : Service -> String
typeToKey service =
    case service of
        AmazonS3 ->
            "AmazonS3"

        Ipfs ->
            "Ipfs"

        Local ->
            "Local"


{-| Service labels.

Maps a service key to a label.

-}
labels : Bool -> List ( String, String )
labels isElectron =
    let
        default =
            [ ( typeToKey AmazonS3, "Amazon S3" )
            , ( typeToKey Ipfs, "IPFS" )
            ]
    in
        if isElectron then
            List.append default [ ( typeToKey Local, "Locally" ) ]
        else
            default
