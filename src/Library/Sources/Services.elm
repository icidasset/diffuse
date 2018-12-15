module Sources.Services exposing (initialData, keyToType, labels, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties, typeToKey)

{-| Service functions used in other modules.
-}

import Http
import Sources exposing (..)
import Sources.Processing exposing (..)
import Sources.Services.AmazonS3 as AmazonS3
import Time



-- FUNCTIONS


initialData : Service -> SourceData
initialData service =
    case service of
        AmazonS3 ->
            AmazonS3.initialData


makeTrackUrl : Service -> Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl service =
    case service of
        AmazonS3 ->
            AmazonS3.makeTrackUrl


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


parseErrorResponse : Service -> String -> String
parseErrorResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseErrorResponse


parsePreparationResponse : Service -> String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parsePreparationResponse


parseTreeResponse : Service -> String -> Marker -> TreeAnswer Marker
parseTreeResponse service =
    case service of
        AmazonS3 ->
            AmazonS3.parseTreeResponse


postProcessTree : Service -> List String -> List String
postProcessTree service =
    case service of
        AmazonS3 ->
            AmazonS3.postProcessTree


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


properties : Service -> List Property
properties service =
    case service of
        AmazonS3 ->
            AmazonS3.properties



-- KEYS & LABELS


keyToType : String -> Maybe Service
keyToType str =
    case str of
        "AmazonS3" ->
            Just AmazonS3

        _ ->
            Nothing


typeToKey : Service -> String
typeToKey service =
    case service of
        AmazonS3 ->
            "AmazonS3"


{-| Service labels.
Maps a service key to a label.
-}
labels : List ( String, String )
labels =
    [ ( typeToKey AmazonS3, "Amazon S3" )
    ]
