module UI.Tracks.State exposing (..)

import Alien
import Common exposing (..)
import ContextMenu
import Coordinates exposing (Coordinates)
import Html.Events.Extra.Mouse as Mouse
import InfiniteList
import Json.Decode as Json
import Json.Encode
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications
import Playlists exposing (Playlist)
import Queue
import Return exposing (andThen, return)
import Return.Ext as Return
import Sources
import Task
import Task.Extra as Task
import Tracks exposing (..)
import Tracks.Collection as Collection
import Tracks.Encoding as Encoding
import Tracks.Favourites as Favourites
import UI.Common.State as Common exposing (showNotification)
import UI.DnD as DnD
import UI.Page
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.Scene.List
import UI.Tracks.Types as Tracks exposing (..)
import UI.Types as UI exposing (Manager, Model, Msg(..))
import UI.User.State.Export as User
import User.Layer exposing (HypaethralData)



-- 📣


update : Tracks.Msg -> Manager
update msg =
    case msg of
        Download a b ->
            download a b

        DownloadFinished ->
            downloadFinished

        Harvest ->
            harvest

        MarkAsSelected a b ->
            markAsSelected a b

        ScrollToNowPlaying ->
            scrollToNowPlaying

        ToggleCachedOnly ->
            toggleCachedOnly

        ToggleFavouritesOnly ->
            toggleFavouritesOnly

        ToggleHideDuplicates ->
            toggleHideDuplicates

        -----------------------------------------
        -- Cache
        -----------------------------------------
        ClearCache ->
            clearCache

        RemoveFromCache a ->
            removeFromCache a

        StoreInCache a ->
            storeInCache a

        StoredInCache a b ->
            storedInCache a b

        -----------------------------------------
        -- Collection
        -----------------------------------------
        Add a ->
            add a

        RemoveByPaths a ->
            removeByPaths a

        RemoveBySourceId a ->
            removeBySourceId a

        SortBy a ->
            sortBy a

        ToggleFavourite a ->
            toggleFavourite a

        -----------------------------------------
        -- Groups
        -----------------------------------------
        DisableGrouping ->
            disableGrouping

        GroupBy a ->
            groupBy a

        -----------------------------------------
        -- Menus
        -----------------------------------------
        ShowTracksMenu a b c ->
            showTracksMenu a b c

        ShowTracksMenuWithSmallDelay a b c ->
            showTracksMenuWithDelay a b c

        ShowViewMenu a b ->
            showViewMenu a b

        -----------------------------------------
        -- Scenes
        -----------------------------------------
        InfiniteListMsg a ->
            infiniteListMsg a

        -----------------------------------------
        -- Search
        -----------------------------------------
        ClearSearch ->
            clearSearch

        Search ->
            search

        SetSearchResults a ->
            setSearchResults a

        SetSearchTerm a ->
            setSearchTerm a



-- 🔱


add : Json.Value -> Manager
add encodedTracks model =
    model
        |> reviseCollection
            (encodedTracks
                |> Json.decodeValue (Json.list Encoding.trackDecoder)
                |> Result.withDefault []
                |> Collection.add
            )
        |> andThen search


clearCache : Manager
clearCache model =
    model.cachedTracks
        |> Json.Encode.list Json.Encode.string
        |> Alien.broadcast Alien.RemoveTracksFromCache
        |> Ports.toBrain
        |> return { model | cachedTracks = [] }
        |> andThen harvest
        |> andThen User.saveEnclosedUserData
        |> andThen
            ("Tracks cache was cleared"
                |> Notifications.warning
                |> Common.showNotification
            )


clearSearch : Manager
clearSearch model =
    { model | searchResults = Nothing, searchTerm = Nothing }
        |> reviseCollection Collection.harvest
        |> andThen User.saveEnclosedUserData


download : String -> List Track -> Manager
download zipName tracks model =
    let
        notification =
            Notifications.stickyWarning "Downloading tracks ..."

        downloading =
            Just { notificationId = Notifications.id notification }
    in
    [ ( "zipName", Json.Encode.string zipName )
    , ( "trackIds"
      , tracks
            |> List.map .id
            |> Json.Encode.list Json.Encode.string
      )
    ]
        |> Json.Encode.object
        |> Alien.broadcast Alien.DownloadTracks
        |> Ports.toBrain
        |> return { model | downloading = downloading }
        |> andThen (Common.showNotification notification)


downloadFinished : Manager
downloadFinished model =
    case model.downloading of
        Just { notificationId } ->
            Common.dismissNotification
                { id = notificationId }
                { model | downloading = Nothing }

        Nothing ->
            Return.singleton model


disableGrouping : Manager
disableGrouping model =
    { model | grouping = Nothing }
        |> reviseCollection Collection.arrange
        |> andThen User.saveEnclosedUserData


failedToStoreInCache : List String -> Manager
failedToStoreInCache trackIds m =
    showNotification
        (Notifications.error "Failed to store track in cache")
        { m | cachingTracksInProgress = List.without trackIds m.cachingTracksInProgress }


finishedStoringInCache : List String -> Manager
finishedStoringInCache trackIds model =
    { model
        | cachedTracks = model.cachedTracks ++ trackIds
        , cachingTracksInProgress = List.without trackIds model.cachingTracksInProgress
    }
        |> (\m ->
                -- When a context menu of a track is open,
                -- it should be "rerendered" in case
                -- the track is no longer being downloaded.
                case m.contextMenu of
                    Just contextMenu ->
                        let
                            isTrackContextMenu =
                                ContextMenu.anyItem
                                    (.label >> (==) "Downloading ...")
                                    contextMenu

                            coordinates =
                                ContextMenu.coordinates contextMenu
                        in
                        if isTrackContextMenu then
                            showTracksMenu Nothing { alt = False } coordinates m

                        else
                            Return.singleton m

                    Nothing ->
                        Return.singleton m
           )
        |> andThen harvest
        |> andThen User.saveEnclosedUserData


groupBy : Tracks.Grouping -> Manager
groupBy grouping model =
    { model | grouping = Just grouping }
        |> reviseCollection Collection.arrange
        |> andThen User.saveEnclosedUserData


harvest : Manager
harvest =
    reviseCollection Collection.harvest


infiniteListMsg : InfiniteList.Model -> Manager
infiniteListMsg infiniteList model =
    Return.singleton { model | infiniteList = infiniteList }


markAsSelected : Int -> { shiftKey : Bool } -> Manager
markAsSelected indexInList { shiftKey } model =
    let
        selection =
            if shiftKey then
                model.selectedTrackIndexes
                    |> List.head
                    |> Maybe.map
                        (\n ->
                            if n > indexInList then
                                List.range indexInList n

                            else
                                List.range n indexInList
                        )
                    |> Maybe.withDefault [ indexInList ]

            else
                [ indexInList ]
    in
    Return.singleton { model | selectedTrackIndexes = selection }


removeByPaths : Json.Value -> Manager
removeByPaths encodedParams model =
    let
        decoder =
            Json.map2
                Tuple.pair
                (Json.field "filePaths" <| Json.list Json.string)
                (Json.field "sourceId" Json.string)

        ( paths, sourceId ) =
            encodedParams
                |> Json.decodeValue decoder
                |> Result.withDefault ( [], missingId )

        { kept, removed } =
            Tracks.removeByPaths
                { sourceId = sourceId, paths = paths }
                model.tracks.untouched

        newCollection =
            { emptyCollection | untouched = kept }
    in
    { model | tracks = newCollection }
        |> reviseCollection Collection.identify
        |> andThen (removeFromCache removed)


removeBySourceId : String -> Manager
removeBySourceId sourceId model =
    let
        { kept, removed } =
            Tracks.removeBySourceId sourceId model.tracks.untouched

        newCollection =
            { emptyCollection | untouched = kept }
    in
    sourceId
        |> Json.Encode.string
        |> Alien.broadcast Alien.RemoveTracksBySourceId
        |> Ports.toBrain
        |> return { model | tracks = newCollection }
        |> andThen (reviseCollection Collection.identify)
        |> andThen (removeFromCache removed)


removeFromCache : List Track -> Manager
removeFromCache tracks model =
    let
        trackIds =
            List.map .id tracks
    in
    trackIds
        |> Json.Encode.list Json.Encode.string
        |> Alien.broadcast Alien.RemoveTracksFromCache
        |> Ports.toBrain
        |> return { model | cachedTracks = List.without trackIds model.cachedTracks }
        |> andThen harvest
        |> andThen User.saveEnclosedUserData


reviseCollection : (Parcel -> Parcel) -> Manager
reviseCollection collector model =
    resolveParcel
        (model
            |> makeParcel
            |> collector
        )
        model


search : Manager
search model =
    case ( model.searchTerm, model.searchResults ) of
        ( Just term, _ ) ->
            term
                |> String.trim
                |> Json.Encode.string
                |> Ports.giveBrain Alien.SearchTracks
                |> return model

        ( Nothing, Just _ ) ->
            reviseCollection Collection.harvest { model | searchResults = Nothing }

        ( Nothing, Nothing ) ->
            Return.singleton model


setSearchResults : Json.Value -> Manager
setSearchResults json model =
    case model.searchTerm of
        Just _ ->
            json
                |> Json.decodeValue (Json.list Json.string)
                |> Result.withDefault []
                |> (\results -> { model | searchResults = Just results })
                |> reviseCollection Collection.harvest
                |> andThen (Common.toggleLoadingScreen Off)

        Nothing ->
            Return.singleton model


setSearchTerm : String -> Manager
setSearchTerm term model =
    User.saveEnclosedUserData
        (case String.trim term of
            "" ->
                { model | searchTerm = Nothing }

            _ ->
                { model | searchTerm = Just term }
        )


showTracksMenu : Maybe Int -> { alt : Bool } -> Coordinates -> Manager
showTracksMenu maybeTrackIndex { alt } coordinates model =
    let
        selection =
            case maybeTrackIndex of
                Just trackIndex ->
                    if List.isEmpty model.selectedTrackIndexes then
                        [ trackIndex ]

                    else if List.member trackIndex model.selectedTrackIndexes == False then
                        [ trackIndex ]

                    else
                        model.selectedTrackIndexes

                Nothing ->
                    model.selectedTrackIndexes

        menuDependencies =
            { cached = model.cachedTracks
            , cachingInProgress = model.cachingTracksInProgress
            , currentTime = model.currentTime
            , selectedPlaylist = model.selectedPlaylist
            , lastModifiedPlaylistName = model.lastModifiedPlaylist
            , showAlternativeMenu = alt
            , sources = model.sources
            }

        tracks =
            List.pickIndexes selection model.tracks.harvested
    in
    coordinates
        |> Tracks.trackMenu menuDependencies tracks
        |> Common.showContextMenuWithModel
            { model
                | dnd = DnD.initialModel
                , selectedTrackIndexes = selection
            }


showTracksMenuWithDelay : Maybe Int -> { alt : Bool } -> Coordinates -> Manager
showTracksMenuWithDelay a b c model =
    Tracks.ShowTracksMenu a b c
        |> TracksMsg
        |> Task.doDelayed 250
        |> return model


showViewMenu : Maybe Grouping -> Mouse.Event -> Manager
showViewMenu maybeGrouping mouseEvent model =
    mouseEvent.clientPos
        |> Coordinates.fromTuple
        |> Tracks.viewMenu model.cachedTracksOnly maybeGrouping
        |> Common.showContextMenuWithModel model


scrollToNowPlaying : Manager
scrollToNowPlaying model =
    model.nowPlaying
        |> Maybe.map
            (.identifiedTrack >> Tuple.second >> .id)
        |> Maybe.andThen
            (\id ->
                List.find
                    (Tuple.second >> .id >> (==) id)
                    model.tracks.harvested
            )
        |> Maybe.map
            (case model.scene of
                List ->
                    UI.Tracks.Scene.List.scrollToNowPlaying model.tracks.harvested
            )
        |> Maybe.map
            (\cmd ->
                cmd
                    |> return model
                    |> andThen (Common.changeUrlUsingPage UI.Page.Index)
            )
        |> Maybe.withDefault
            (Return.singleton model)


sortBy : SortBy -> Manager
sortBy property model =
    let
        sortDir =
            if model.sortBy /= property then
                Asc

            else if model.sortDirection == Asc then
                Desc

            else
                Asc
    in
    { model | sortBy = property, sortDirection = sortDir }
        |> reviseCollection Collection.arrange
        |> andThen User.saveEnclosedUserData


storeInCache : List Track -> Manager
storeInCache tracks model =
    let
        trackIds =
            List.map .id tracks

        notification =
            case tracks of
                [ t ] ->
                    ("__" ++ t.tags.title ++ "__ will be stored in the cache")
                        |> Notifications.success

                list ->
                    list
                        |> List.length
                        |> String.fromInt
                        |> (\s -> "__" ++ s ++ " tracks__ will be stored in the cache")
                        |> Notifications.success
    in
    tracks
        |> Json.Encode.list
            (\track ->
                Json.Encode.object
                    [ ( "trackId"
                      , Json.Encode.string track.id
                      )
                    , ( "url"
                      , track
                            |> Queue.makeTrackUrl
                                model.currentTime
                                model.sources
                            |> Json.Encode.string
                      )
                    ]
            )
        |> Alien.broadcast Alien.StoreTracksInCache
        |> Ports.toBrain
        |> return { model | cachingTracksInProgress = model.cachingTracksInProgress ++ trackIds }
        |> andThen (Common.showNotification notification)


storedInCache : Json.Value -> Maybe String -> Manager
storedInCache json maybeError =
    case
        ( maybeError
        , Json.decodeValue (Json.list Json.string) json
        )
    of
        ( Nothing, Ok list ) ->
            finishedStoringInCache list

        ( Nothing, Err err ) ->
            err
                |> Json.errorToString
                |> Notifications.error
                |> Common.showNotification

        ( Just _, Ok trackIds ) ->
            failedToStoreInCache trackIds

        ( Just err, Err _ ) ->
            err
                |> Notifications.error
                |> Common.showNotification


toggleCachedOnly : Manager
toggleCachedOnly model =
    { model | cachedTracksOnly = not model.cachedTracksOnly }
        |> reviseCollection Collection.harvest
        |> andThen User.saveEnclosedUserData
        |> andThen Common.forceTracksRerender


toggleFavourite : Int -> Manager
toggleFavourite index model =
    case List.getAt index model.tracks.harvested of
        Just ( i, t ) ->
            let
                newFavourites =
                    Favourites.toggleInFavouritesList ( i, t ) model.favourites

                effect =
                    if model.favouritesOnly then
                        Collection.map (Favourites.toggleInTracksList t) >> Collection.harvest

                    else
                        Collection.map (Favourites.toggleInTracksList t)
            in
            { model | favourites = newFavourites }
                |> reviseCollection effect
                |> andThen User.saveFavourites

        Nothing ->
            Return.singleton model


toggleFavouritesOnly : Manager
toggleFavouritesOnly model =
    { model | favouritesOnly = not model.favouritesOnly }
        |> reviseCollection Collection.harvest
        |> andThen User.saveEnclosedUserData


toggleHideDuplicates : Manager
toggleHideDuplicates model =
    { model | hideDuplicates = not model.hideDuplicates }
        |> reviseCollection Collection.arrange
        |> andThen User.saveSettings



-- 📣  ░░  PARCEL


makeParcel : Model -> Parcel
makeParcel model =
    ( { cached = model.cachedTracks
      , cachedOnly = model.cachedTracksOnly
      , enabledSourceIds = Sources.enabledSourceIds model.sources
      , favourites = model.favourites
      , favouritesOnly = model.favouritesOnly
      , grouping = model.grouping
      , hideDuplicates = model.hideDuplicates
      , searchResults = model.searchResults
      , selectedPlaylist = model.selectedPlaylist
      , sortBy = model.sortBy
      , sortDirection = model.sortDirection
      }
    , model.tracks
    )


resolveParcel : Parcel -> Manager
resolveParcel ( deps, newCollection ) model =
    let
        scrollObj =
            Json.Encode.object
                [ ( "scrollTop", Json.Encode.int 0 ) ]

        scrollEvent =
            Json.Encode.object
                [ ( "target", scrollObj ) ]

        newScrollContext =
            scrollContext model

        collectionChanged =
            Collection.tracksChanged
                model.tracks.untouched
                newCollection.untouched

        harvestChanged =
            Collection.harvestChanged
                model.tracks.harvested
                newCollection.harvested

        searchChanged =
            newScrollContext /= model.tracks.scrollContext

        modelWithNewCollection =
            (if model.scene == List && searchChanged then
                \m -> { m | infiniteList = InfiniteList.updateScroll scrollEvent m.infiniteList }

             else
                identity
            )
                { model
                    | tracks =
                        { newCollection | scrollContext = newScrollContext }
                    , selectedTrackIndexes =
                        if collectionChanged || harvestChanged then
                            []

                        else
                            model.selectedTrackIndexes
                }
    in
    (if collectionChanged then
        andThen Common.generateDirectoryPlaylists >> andThen Queue.reset

     else if harvestChanged then
        andThen Queue.reset

     else
        identity
    )
        ( modelWithNewCollection
          -----------------------------------------
          -- Command
          -----------------------------------------
        , if searchChanged then
            case model.scene of
                List ->
                    UI.Tracks.Scene.List.scrollToTop

          else
            Cmd.none
        )


scrollContext : Model -> String
scrollContext model =
    String.concat
        [ Maybe.withDefault "" <| model.searchTerm
        , Maybe.withDefault "" <| Maybe.map .name model.selectedPlaylist
        ]



-- 📣  ░░  USER DATA


importHypaethral : HypaethralData -> Maybe Playlist -> Manager
importHypaethral data selectedPlaylist model =
    let
        adjustedModel =
            { model
                | favourites = data.favourites
                , hideDuplicates = Maybe.unwrap False .hideDuplicates data.settings
                , selectedPlaylist = selectedPlaylist
                , tracks = { emptyCollection | untouched = data.tracks }
            }
    in
    adjustedModel
        |> resolveParcel
            (adjustedModel
                |> makeParcel
                |> Collection.identify
            )
        |> andThen search
        |> (case model.searchTerm of
                Just _ ->
                    identity

                Nothing ->
                    andThen (Common.toggleLoadingScreen Off)
           )
