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
    string
        |> single
        |> path [ "Key" ]
        |> list
        |> path [ "Contents" ]


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
        (maybe <| path [ "IsTruncated" ] <| single string)
        (maybe <| path [ "NextContinuationToken" ] <| single string)



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
