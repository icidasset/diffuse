module Sources.Services.Azure.BlobParser exposing (parseErrorResponse, parseTreeResponse)

import Sources.Processing exposing (Marker(..), TreeAnswer)
import Xml.Decode exposing (..)



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response _ =
    response
        |> decodeString
            (map2
                (\f m -> { filePaths = f, marker = m })
                filePathsDecoder
                markerDecoder
            )
        |> Result.withDefault { filePaths = [], marker = TheEnd }


filePathsDecoder : Decoder (List String)
filePathsDecoder =
    string
        |> single
        |> path [ "Name" ]
        |> list
        |> path [ "Blobs", "Blob" ]


markerDecoder : Decoder Marker
markerDecoder =
    map
        (\maybeNextMarker ->
            case maybeNextMarker of
                Just "" ->
                    TheEnd

                Just nextMarker ->
                    InProgress nextMarker

                Nothing ->
                    TheEnd
        )
        (maybe <| path [ "NextMarker" ] <| single string)



-- ERROR


parseErrorResponse : String -> Maybe String
parseErrorResponse response =
    response
        |> decodeString errorMessagesDecoder
        |> Result.toMaybe


errorMessagesDecoder : Decoder String
errorMessagesDecoder =
    string
        |> single
        |> path [ "Message" ]
