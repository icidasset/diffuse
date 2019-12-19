module Sources.Services.WebDav.Parser exposing (..)

import Maybe.Extra as Maybe
import Sources.Processing exposing (Marker(..), TreeAnswer)
import Sources.Services.Ipfs.Marker as Marker
import String.Ext as String
import Url
import Xml.Decode exposing (..)



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        currentDir =
            Maybe.withDefault "//" (Marker.takeOne previousMarker)

        entries =
            response
                |> String.replace "<d:" "<D:"
                |> String.replace "</d:" "</D:"
                |> decodeString treeDecoder
                |> Result.withDefault []
                |> List.map Url.percentDecode
                |> Maybe.values
                |> List.filter ((/=) currentDir)

        ( dirs, files ) =
            List.partition (String.endsWith "/") entries
    in
    { filePaths =
        List.map (String.chopStart "/") files
    , marker =
        previousMarker
            |> Marker.removeOne
            |> Marker.concat dirs
    }


treeDecoder : Decoder (List String)
treeDecoder =
    path [ "D:response" ] (leakyList treeItemDecoder)


treeItemDecoder : Decoder String
treeItemDecoder =
    map2
        (\_ h -> h)
        (oneOf
            [ -- Audio
              --------
              string
                |> single
                |> path [ "D:propstat", "D:prop", "D:getcontenttype" ]
                |> andThen mustBeAudio

            -- Directory
            ------------
            , string
                |> single
                |> path [ "D:propstat", "D:prop", "D:resourcetype", "D:collection" ]
            ]
        )
        (path [ "D:href" ] (single string))


mustBeAudio : String -> Decoder String
mustBeAudio contentType =
    if String.startsWith "audio/" contentType then
        succeed contentType

    else
        fail "Ignore this, not an audio file"



-- ERROR


parseErrorResponse : String -> Maybe String
parseErrorResponse =
    Just
