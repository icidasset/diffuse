module Sources.Services.Ipfs.Parser exposing (..)

import Json.Decode exposing (..)
import Sources.Services.Ipfs.Marker as Marker
import Sources.Types exposing (Marker(..), ParsedResponse)
import Sources.Utils exposing (isMusicFile)


-- Tree


parseTreeResponse : String -> Marker -> ParsedResponse Marker
parseTreeResponse response previousMarker =
    let
        links =
            case decodeString treeDecoder response of
                Ok l ->
                    l

                Err _ ->
                    []

        dirs =
            links
                |> List.filter (.typ >> (==) 1)
                |> List.map .hash

        files =
            links
                |> List.filter (.typ >> (==) 2)
                |> List.filter (.name >> isMusicFile)
                |> List.map .hash
    in
        { filePaths =
            files
        , marker =
            previousMarker
                |> Marker.removeOne
                |> Marker.concat dirs
        }


treeDecoder : Decoder (List Link)
treeDecoder =
    field "Objects" <| index 0 <| field "Links" <| list linkDecoder



-- Links


type alias Link =
    { hash : String
    , name : String
    , typ : Int
    }


linkDecoder : Decoder Link
linkDecoder =
    map3 Link
        (field "Hash" string)
        (field "Name" string)
        (field "Type" int)
