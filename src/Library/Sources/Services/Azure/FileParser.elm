module Sources.Services.Azure.FileParser exposing (parseErrorResponse, parseTreeResponse)

import Sources.Processing exposing (Marker(..), TreeAnswer)
import Sources.Services.Azure.FileMarker as FileMarker exposing (MarkerItem(..))
import Sources.Services.Common exposing (cleanPath)
import Xml.Decode exposing (..)



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    response
        |> decodeString (treeDecoder previousMarker)
        |> Result.withDefault { filePaths = [], marker = TheEnd }


treeDecoder : Marker -> Decoder (TreeAnswer Marker)
treeDecoder previousMarker =
    usedDirectoryDecoder
        |> map cleanPath
        |> andThen
            (\usedDirectory ->
                map2
                    (\a b -> ( usedDirectory, a, b ))
                    (map (List.map <| String.append usedDirectory) filePathsDecoder)
                    (map (List.map <| String.append usedDirectory) directoryPathsDecoder)
            )
        |> andThen
            (\( usedDirectory, filePaths, directoryPaths ) ->
                previousMarker
                    |> FileMarker.removeOne
                    |> FileMarker.concat (List.map Directory directoryPaths)
                    |> markerDecoder usedDirectory
                    |> map (\marker -> { filePaths = filePaths, marker = marker })
            )


usedDirectoryDecoder : Decoder String
usedDirectoryDecoder =
    stringAttr "DirectoryPath"


filePathsDecoder : Decoder (List String)
filePathsDecoder =
    string
        |> single
        |> path [ "Name" ]
        |> list
        |> path [ "Entries", "File" ]


directoryPathsDecoder : Decoder (List String)
directoryPathsDecoder =
    string
        |> single
        |> path [ "Name" ]
        |> list
        |> path [ "Entries", "Directory" ]


markerDecoder : String -> Marker -> Decoder Marker
markerDecoder usedDirectory markerWithDirectories =
    map
        (\maybeNextMarker ->
            case maybeNextMarker of
                Just "" ->
                    markerWithDirectories

                Just marker ->
                    FileMarker.concat
                        [ Param { directory = usedDirectory, marker = marker } ]
                        markerWithDirectories

                Nothing ->
                    markerWithDirectories
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
