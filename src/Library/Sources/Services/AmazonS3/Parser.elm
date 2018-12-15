module Sources.Services.AmazonS3.Parser exposing (parseErrorResponse, parseTreeResponse)

import Conditional exposing (..)
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
    path [ "Contents" ] (list <| path [ "Key" ] (single string))


markerDecoder : Decoder Marker
markerDecoder =
    map2
        (\a b ->
            Maybe.withDefault
                TheEnd
                (Maybe.map2
                    (\isTruncated token ->
                        ifThenElse (isTruncated == "true") (InProgress token) TheEnd
                    )
                    a
                    b
                )
        )
        (maybe <| path [ "IsTruncated" ] (single string))
        (maybe <| path [ "NextContinuationToken" ] (single string))



-- ERROR


parseErrorResponse : String -> String
parseErrorResponse response =
    response
        |> decodeString errorMessagesDecoder
        |> Result.toMaybe
        |> Maybe.andThen List.head
        |> Maybe.withDefault "Invalid request"


errorMessagesDecoder : Decoder (List String)
errorMessagesDecoder =
    path [ "Error" ] (list <| path [ "Message" ] (single string))
