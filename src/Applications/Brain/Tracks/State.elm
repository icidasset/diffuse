module Brain.Tracks.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Ports as Ports
import Brain.Types exposing (..)
import Brain.User.State as User
import Json.Decode as Json exposing (Decoder)
import Json.Encode
import Queue
import Return exposing (andThen, return)
import Return.Ext as Return
import Tracks exposing (Track)
import Tracks.Encoding



-- 🔱


add : List Track -> Manager
add list model =
    case list of
        [] ->
            Return.singleton model

        tracks ->
            model
                |> User.saveTracksAndUpdateSearchIndex
                    (List.append model.hypaethralUserData.tracks tracks)
                |> andThen
                    (tracks
                        |> Json.Encode.list Tracks.Encoding.encodeTrack
                        |> Common.giveUI Alien.AddTracks
                    )


download : Json.Value -> Manager
download json model =
    let
        ( zipName, trackIds ) =
            json
                |> Json.decodeValue downloadParamsDecoder
                |> Result.withDefault ( "?", [] )
    in
    model.hypaethralUserData.tracks
        |> Tracks.pick trackIds
        |> List.indexedMap Tuple.pair
        |> Json.Encode.list
            (\( idx, track ) ->
                Json.Encode.object
                    [ ( "filename"
                      , [ (idx + 1)
                            |> String.fromInt
                            |> String.padLeft 2 '0'
                        , " - "
                        , track.tags.artist
                        , " - "
                        , track.tags.title
                        ]
                            |> String.concat
                            |> Json.Encode.string
                      )

                    --
                    , ( "url"
                      , track
                            |> Queue.makeTrackUrl
                                model.currentTime
                                model.hypaethralUserData.sources
                            |> Json.Encode.string
                      )
                    ]
            )
        |> (\encodedTracks ->
                Json.Encode.object
                    [ ( "name", Json.Encode.string zipName )
                    , ( "tracks", encodedTracks )
                    ]
           )
        |> Ports.downloadTracks
        |> return model


gotSearchResults : List String -> Manager
gotSearchResults results =
    Common.giveUI Alien.SearchTracks (Json.Encode.list Json.Encode.string results)


removeByPaths : { sourceId : String, paths : List String } -> Manager
removeByPaths args model =
    User.saveTracksAndUpdateSearchIndex
        (model.hypaethralUserData.tracks
            |> Tracks.removeByPaths args
            |> .kept
        )
        model


removeBySourceId : Json.Value -> Manager
removeBySourceId data model =
    case Json.decodeValue Json.string data of
        Ok sourceId ->
            User.saveTracksAndUpdateSearchIndex
                (model.hypaethralUserData.tracks
                    |> Tracks.removeBySourceId sourceId
                    |> .kept
                )
                model

        Err _ ->
            Return.singleton model


removeFromCache : Json.Value -> Manager
removeFromCache data =
    Return.communicate (Ports.removeTracksFromCache data)


search : Json.Value -> Manager
search encodedSearchTerm =
    encodedSearchTerm
        |> Json.decodeValue Json.string
        |> Result.map Ports.requestSearch
        |> Result.withDefault Cmd.none
        |> Return.communicate


storeInCache : Json.Value -> Manager
storeInCache data =
    Return.communicate (Ports.storeTracksInCache data)


updateSearchIndex : Json.Value -> Manager
updateSearchIndex data =
    Return.communicate (Ports.updateSearchIndex data)



-- ⚗️


downloadParamsDecoder : Decoder ( String, List String )
downloadParamsDecoder =
    Json.map2
        Tuple.pair
        (Json.field "zipName" <| Json.string)
        (Json.field "trackIds" <| Json.list Json.string)
