module Brain.Tracks.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Ports as Ports
import Brain.Types exposing (..)
import Brain.User.State as User
import Dict
import Dict.Ext as Dict
import Json.Decode as Json exposing (Decoder)
import Json.Encode
import List.Extra as List
import Queue
import Return exposing (andThen, return)
import Return.Ext as Return
import Sources exposing (Source)
import Sources.Processing exposing (ContextForTagsSync, HttpMethod(..), TagUrls)
import Sources.Services
import Time
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
        { prefixTrackNumber, trackIds, zipName } =
            json
                |> Json.decodeValue downloadParamsDecoder
                |> Result.withDefault
                    { prefixTrackNumber = False
                    , trackIds = []
                    , zipName = "failed-to-decode-json"
                    }
    in
    model.hypaethralUserData.tracks
        |> Tracks.pick trackIds
        |> List.indexedMap Tuple.pair
        |> Json.Encode.list
            (\( idx, track ) ->
                Json.Encode.object
                    [ ( "filename"
                      , [ if prefixTrackNumber then
                            (idx + 1)
                                |> String.fromInt
                                |> String.padLeft 2 '0'
                                |> (\s -> s ++ " - ")

                          else
                            ""
                        , track.tags.artist
                            |> Maybe.map (\a -> a ++ " - ")
                            |> Maybe.withDefault ""
                        , track.tags.title
                        ]
                            |> String.concat
                            |> Json.Encode.string
                      )

                    --
                    , ( "path"
                      , Json.Encode.string track.path
                      )
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


makeArtworkTrackUrls : Json.Value -> Manager
makeArtworkTrackUrls json model =
    json
        |> Json.decodeValue
            (Json.dict Json.string)
        |> Result.map
            (\dict ->
                let
                    maybeSource =
                        Maybe.andThen
                            (\trackSourceId ->
                                List.find
                                    (.id >> (==) trackSourceId)
                                    model.hypaethralUserData.sources
                            )
                            (Dict.get "trackSourceId" dict)

                    trackPath =
                        Dict.fetch "trackPath" "" dict

                    mkTrackUrl =
                        makeTrackUrl model.currentTime trackPath maybeSource
                in
                dict
                    |> Dict.insert "trackGetUrl" (mkTrackUrl Get)
                    |> Dict.insert "trackHeadUrl" (mkTrackUrl Head)
                    |> Json.Encode.dict identity Json.Encode.string
                    |> Ports.provideArtworkTrackUrls
                    |> return model
            )
        |> Result.withDefault
            (Return.singleton model)


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


replaceTags : ContextForTagsSync -> Manager
replaceTags context model =
    model.hypaethralUserData.tracks
        |> List.foldr
            (\track ( acc, trackIds, tags ) ->
                case List.elemIndex track.id trackIds of
                    Just idx ->
                        let
                            newTags =
                                tags
                                    |> List.getAt idx
                                    |> Maybe.andThen identity
                                    |> Maybe.withDefault track.tags
                        in
                        ( { track | tags = newTags } :: acc
                        , List.removeAt idx trackIds
                        , List.removeAt idx tags
                        )

                    Nothing ->
                        ( track :: acc
                        , trackIds
                        , tags
                        )
            )
            ( []
            , context.trackIds
            , context.receivedTags
            )
        |> (\( a, _, _ ) ->
                User.saveTracksAndUpdateSearchIndex a model
           )
        |> andThen
            (\m ->
                m.hypaethralUserData.tracks
                    |> Json.Encode.list Tracks.Encoding.encodeTrack
                    |> (\data -> Common.giveUI Alien.ReloadTracks data m)
            )


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


syncTrackTags : Json.Value -> Manager
syncTrackTags data model =
    let
        result =
            Json.decodeValue
                (Json.list <|
                    Json.map3
                        (\path sourceId trackId ->
                            { path = path
                            , sourceId = sourceId
                            , trackId = trackId
                            }
                        )
                        (Json.field "path" Json.string)
                        (Json.field "sourceId" Json.string)
                        (Json.field "trackId" Json.string)
                )
                data

        ( sources, _ ) =
            result
                |> Result.withDefault []
                |> List.foldl
                    (\{ sourceId } ( dict, acc ) ->
                        if List.member sourceId acc then
                            ( dict, acc )

                        else
                            case List.find (.id >> (==) sourceId) model.hypaethralUserData.sources of
                                Just source ->
                                    ( Dict.insert sourceId source dict, sourceId :: acc )

                                Nothing ->
                                    ( dict, sourceId :: acc )
                    )
                    ( Dict.empty, [] )
    in
    case result of
        Ok list ->
            list
                |> List.foldr
                    (\{ path, sourceId, trackId } ( accPaths, accUrls, accIds ) ->
                        sources
                            |> Dict.get sourceId
                            |> Maybe.map
                                (tagUrls model.currentTime path)
                            |> Maybe.map
                                (\urls ->
                                    ( path :: accPaths, urls :: accUrls, trackId :: accIds )
                                )
                            |> Maybe.withDefault
                                ( accPaths, accUrls, accIds )
                    )
                    ( [], [], [] )
                |> (\( accPaths, accUrls, accIds ) ->
                        Ports.syncTags
                            { receivedFilePaths = accPaths
                            , receivedTags = []
                            , trackIds = accIds
                            , urlsForTags = accUrls
                            }
                   )
                |> return model

        Err _ ->
            Return.singleton model


updateSearchIndex : Json.Value -> Manager
updateSearchIndex data =
    Return.communicate (Ports.updateSearchIndex data)



-- ⚗️


downloadParamsDecoder :
    Decoder
        { prefixTrackNumber : Bool
        , trackIds : List String
        , zipName : String
        }
downloadParamsDecoder =
    Json.map3
        (\a b c ->
            { prefixTrackNumber = a
            , trackIds = b
            , zipName = c
            }
        )
        (Json.field "prefixTrackNumber" <| Json.bool)
        (Json.field "trackIds" <| Json.list Json.string)
        (Json.field "zipName" <| Json.string)


makeTrackUrl : Time.Posix -> String -> Maybe Source -> HttpMethod -> String
makeTrackUrl timestamp trackPath maybeSource httpMethod =
    case maybeSource of
        Just source ->
            Sources.Services.makeTrackUrl
                source.service
                timestamp
                source.id
                source.data
                httpMethod
                trackPath

        Nothing ->
            "<missing-source>"


tagUrls : Time.Posix -> String -> Source -> TagUrls
tagUrls currentTime path source =
    let
        maker =
            Sources.Services.makeTrackUrl source.service currentTime source.id source.data
    in
    { getUrl = maker Get path
    , headUrl = maker Head path
    }
