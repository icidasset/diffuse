module Sources.Services.WebDav.Parser exposing (parseTreeResponse, parseErrorResponse)

import Maybe.Extra as Maybe
import Sources.Pick exposing (isMusicFile)
import Sources.Processing.Types exposing (Marker(..), TreeAnswer)
import Sources.Services.Utils exposing (unescapeXmlEntities)
import Sources.Services.WebDav.Marker as Marker
import Xml
import Xml.Decode as Xml
import Xml.Encode as Xml
import Xml.Query exposing (..)


-- Tree


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        decodedXml =
            response
                |> Xml.decode
                |> Result.toMaybe
                |> Maybe.map (tags "d:multistatus")
                |> Maybe.andThen List.head
                |> Maybe.withDefault Xml.null

        rootPlusResponses =
            tags "d:response" decodedXml

        responses =
            List.drop 1 rootPlusResponses

        dirs =
            responses
                |> List.filter isDir
                |> collect (tag "d:href" string)
                |> List.map unescapeXmlEntities

        files =
            responses
                |> List.filter isAudioFile
                |> collect (tag "d:href" string)
                |> List.map unescapeXmlEntities
                |> List.filter isMusicFile
    in
        { filePaths =
            files
        , marker =
            previousMarker
                |> Marker.removeOne
                |> Marker.concat dirs
        }


isDir : Xml.Value -> Bool
isDir val =
    val
        |> nested [ "d:propstat", "d:prop", "d:resourcetype", "d:collection" ]
        |> Result.toMaybe
        |> Maybe.isJust


isAudioFile : Xml.Value -> Bool
isAudioFile val =
    val
        |> nested [ "d:propstat", "d:prop", "d:getcontenttype" ]
        |> Result.andThen Xml.Query.string
        |> Result.map (String.startsWith "audio/")
        |> (==) (Ok True)


nested : List String -> Xml.Value -> Result String Xml.Value
nested keys startingValue =
    List.foldl
        (\key acc -> Result.andThen (tag key Ok) acc)
        (Ok startingValue)
        keys



-- Error


parseErrorResponse : String -> String
parseErrorResponse =
    identity
