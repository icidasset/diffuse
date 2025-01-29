module UI.Tracks.State exposing (..)

import Alien
import Base64
import Common exposing (..)
import ContextMenu
import Coordinates exposing (Coordinates)
import Debouncer.Basic as Debouncer
import Dict
import Html.Events.Extra.Mouse as Mouse
import InfiniteList
import Json.Decode as Json
import Json.Encode
import Keyboard
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications
import Playlists exposing (Playlist)
import Queue
import Return exposing (andThen, return)
import Return.Ext as Return
import Sources
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
import UI.Theme
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.Covers as Covers
import UI.Tracks.Types as Tracks exposing (..)
import UI.Types exposing (Manager, Model, Msg(..))
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

        SyncTags a ->
            syncTags a

        ToggleCachedOnly ->
            toggleCachedOnly

        ToggleCoverSelectionReducesPool ->
            toggleCoverSelectionReducesPool

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

        ---------
        -- Covers
        ---------
        GotCachedCover a ->
            gotCachedCover a

        InsertCoverCache a ->
            insertCoverCache a

        -----------------------------------------
        -- Collection
        -----------------------------------------
        Add a ->
            add a

        AddFavourites a ->
            addFavourites a

        Reload a ->
            reload a

        RemoveByPaths a ->
            removeByPaths a

        RemoveBySourceId a ->
            removeBySourceId a

        RemoveFavourites a ->
            removeFavourites a

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
        ShowCoverMenu a b ->
            showCoverMenu a b

        ShowCoverMenuWithSmallDelay a b ->
            showCoverMenuWithDelay a b

        ShowTracksMenu a b c ->
            showTracksMenu a b c

        ShowTracksMenuWithSmallDelay a b c ->
            showTracksMenuWithDelay a b c

        ShowViewMenu a b ->
            showViewMenu a b

        -----------------------------------------
        -- Scenes
        -----------------------------------------
        ChangeScene a ->
            changeScene a

        DeselectCover ->
            deselectCover

        InfiniteListMsg a ->
            infiniteListMsg a

        SelectCover a ->
            selectCover a

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
    reviseCollection
        (encodedTracks
            |> Json.decodeValue (Json.list Encoding.trackDecoder)
            |> Result.withDefault []
            |> Collection.add
        )
        model


addFavourites : List IdentifiedTrack -> Manager
addFavourites =
    manageFavourites AddToFavourites


afterInitialLoad : Manager
afterInitialLoad model =
    Common.toggleLoadingScreen Off model


changeScene : Scene -> Manager
changeScene scene model =
    (case scene of
        Covers ->
            Ports.loadAlbumCovers { list = True, coverView = True }

        List ->
            Cmd.none
    )
        |> return { model | scene = scene, selectedCover = Nothing }
        |> andThen
            (if model.coverSelectionReducesPool then
                Queue.reset

             else
                Return.singleton
            )
        |> andThen Common.forceTracksRerender
        |> andThen User.saveEnclosedUserData


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
                |> Notifications.casual
                |> Common.showNotification
            )


clearSearch : Manager
clearSearch model =
    { model | searchResults = Nothing, searchTerm = Nothing }
        |> reviseCollection Collection.harvest
        |> andThen User.saveEnclosedUserData


deselectCover : Manager
deselectCover model =
    (if model.coverSelectionReducesPool then
        Queue.reset

     else
        Return.singleton
    )
        { model | selectedCover = Nothing }


download : { prefixTrackNumber : Bool, zipName : String } -> List Track -> Manager
download { prefixTrackNumber, zipName } tracks model =
    let
        notification =
            Notifications.stickyCasual "Downloading tracks ..."

        downloading =
            Just { notificationId = Notifications.id notification }
    in
    [ ( "prefixTrackNumber", Json.Encode.bool prefixTrackNumber )
    , ( "trackIds"
      , tracks
            |> List.map .id
            |> Json.Encode.list Json.Encode.string
      )
    , ( "zipName", Json.Encode.string zipName )
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


generateCovers : Manager
generateCovers model =
    model.tracks
        |> Covers.generate model.sortBy
        |> (\c -> { model | covers = c })
        |> Return.singleton


gotCachedCover : Json.Value -> Manager
gotCachedCover json model =
    let
        cachedCovers =
            Maybe.withDefault Dict.empty model.cachedCovers

        decodedValue =
            Json.decodeValue
                (Json.map3
                    (\i k u -> ( i, k, u ))
                    (Json.field "imageType" Json.string)
                    (Json.field "key" Json.string)
                    (Json.field "url" Json.string)
                )
                json
    in
    decodedValue
        |> Result.map (\( _, key, url ) -> Dict.insert key url cachedCovers)
        |> Result.map (\dict -> { model | cachedCovers = Just dict })
        |> Result.withDefault model
        |> (\m ->
                case ( m.nowPlaying, decodedValue ) of
                    ( Just nowPlaying, Ok val ) ->
                        let
                            ( imageType, key, url ) =
                                val

                            ( _, track ) =
                                nowPlaying.item.identifiedTrack

                            hasntLoadedYet =
                                nowPlaying.coverLoaded == False

                            ( keyA, keyB ) =
                                ( Base64.encode (Tracks.coverKey False track)
                                , Base64.encode (Tracks.coverKey True track)
                                )

                            keyMatches =
                                keyA == key || keyB == key
                        in
                        if hasntLoadedYet && keyMatches then
                            ( m, Ports.setMediaSessionArtwork { blobUrl = url, imageType = imageType } )

                        else
                            Return.singleton m

                    _ ->
                        Return.singleton m
           )


groupBy : Tracks.Grouping -> Manager
groupBy grouping model =
    { model | grouping = Just grouping }
        |> reviseCollection Collection.arrange
        |> andThen User.saveEnclosedUserData


harvest : Manager
harvest =
    reviseCollection Collection.harvest


harvestCovers : Manager
harvestCovers model =
    model.covers
        |> Covers.harvest model.selectedCover model.sortBy model.tracks
        |> (\( c, s ) -> { model | covers = c, selectedCover = s })
        |> Return.communicate (Ports.loadAlbumCovers { list = True, coverView = True })


infiniteListMsg : InfiniteList.Model -> Manager
infiniteListMsg infiniteList model =
    return
        { model | infiniteList = infiniteList }
        (Ports.loadAlbumCovers { list = True, coverView = False })


insertCoverCache : Json.Value -> Manager
insertCoverCache json model =
    json
        |> Json.decodeValue (Json.dict Json.string)
        |> Result.map (\dict -> { model | cachedCovers = Just dict })
        |> Result.withDefault model
        |> Return.singleton


manageFavourites : FavouritesManagementAction -> List IdentifiedTrack -> Manager
manageFavourites action tracks model =
    let
        newFavourites =
            (case action of
                AddToFavourites ->
                    Favourites.completeFavouritesList

                RemoveFromFavourites ->
                    Favourites.removeFromFavouritesList
            )
                tracks
                model.favourites

        effect collection =
            collection
                |> Collection.map
                    (case action of
                        AddToFavourites ->
                            Favourites.completeTracksList tracks

                        RemoveFromFavourites ->
                            Favourites.removeFromTracksList tracks
                    )
                |> (if model.favouritesOnly then
                        Collection.harvest

                    else
                        identity
                   )

        selectedCover =
            Maybe.map
                (\cover ->
                    cover.tracks
                        |> (case action of
                                AddToFavourites ->
                                    Favourites.completeTracksList tracks

                                RemoveFromFavourites ->
                                    Favourites.removeFromTracksList tracks
                           )
                        |> (\a -> { cover | tracks = a })
                )
                model.selectedCover
    in
    { model | favourites = newFavourites, selectedCover = selectedCover }
        |> reviseCollection effect
        |> andThen User.saveFavourites
        |> (if model.scene == Covers then
                andThen generateCovers >> andThen harvestCovers

            else
                identity
           )


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


reload : Json.Value -> Manager
reload encodedTracks model =
    reviseCollection
        (encodedTracks
            |> Json.decodeValue (Json.list Encoding.trackDecoder)
            |> Result.withDefault model.tracks.untouched
            |> Collection.replace
        )
        model


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


removeFavourites : List IdentifiedTrack -> Manager
removeFavourites =
    manageFavourites RemoveFromFavourites


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


selectCover : Cover -> Manager
selectCover cover model =
    { model | selectedCover = Just cover }
        |> (if model.coverSelectionReducesPool then
                Queue.reset

            else
                Return.singleton
           )
        |> Return.command (Ports.loadAlbumCovers { list = False, coverView = True })


setSearchResults : Json.Value -> Manager
setSearchResults json model =
    case model.searchTerm of
        Just _ ->
            json
                |> Json.decodeValue (Json.list Json.string)
                |> Result.withDefault []
                |> (\results -> { model | searchResults = Just results })
                |> reviseCollection Collection.harvest
                |> andThen afterInitialLoad

        Nothing ->
            Return.singleton model


setSearchTerm : String -> Manager
setSearchTerm term model =
    (case String.trim term of
        "" ->
            { model | searchTerm = Nothing }

        _ ->
            { model | searchTerm = Just term }
    )
        |> Return.communicate
            (Search
                |> TracksMsg
                |> Debouncer.provideInput
                |> SearchDebounce
                |> Task.do
            )
        |> Return.andThen User.saveEnclosedUserData


showCoverMenu : Cover -> Coordinates -> Manager
showCoverMenu cover coordinates model =
    let
        menuDependencies =
            { cached = model.cachedTracks
            , cachingInProgress = model.cachingTracksInProgress
            , currentTime = model.currentTime
            , selectedPlaylist = model.selectedPlaylist
            , lastModifiedPlaylistName = model.lastModifiedPlaylist
            , showAlternativeMenu = False
            , sources = model.sources
            }
    in
    coordinates
        |> Tracks.trackMenu menuDependencies cover.tracks
        |> Common.showContextMenuWithModel model


showCoverMenuWithDelay : Cover -> Coordinates -> Manager
showCoverMenuWithDelay a b model =
    Tracks.ShowCoverMenu a b
        |> TracksMsg
        |> Task.doDelayed 250
        |> return model


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
            (.item >> .identifiedTrack >> Tuple.second >> .id)
        |> Maybe.andThen
            (\id ->
                List.find
                    (Tuple.second >> .id >> (==) id)
                    model.tracks.harvested
            )
        |> Maybe.map
            (\( identifiers, track ) ->
                case model.scene of
                    Covers ->
                        if List.member Keyboard.Shift model.pressedKeys then
                            return
                                { model | selectedCover = Nothing }
                                (UI.Theme.scrollToNowPlaying Covers ( identifiers, track ) model)

                        else
                            model.covers.harvested
                                |> List.find (\cover -> List.member track.id cover.trackIds)
                                |> Maybe.unwrap model (\cover -> { model | selectedCover = Just cover })
                                |> Return.communicate (Ports.loadAlbumCovers { list = True, coverView = True })

                    List ->
                        return
                            { model | selectedCover = Nothing }
                            (UI.Theme.scrollToNowPlaying List ( identifiers, track ) model)
            )
        |> Maybe.map
            (UI.Page.Index
                |> Common.changeUrlUsingPage
                |> andThen
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
                        |> Notifications.casual

                list ->
                    list
                        |> List.length
                        |> String.fromInt
                        |> (\s -> "__" ++ s ++ " tracks__ will be stored in the cache")
                        |> Notifications.casual
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


syncTags : List Track -> Manager
syncTags tracks =
    tracks
        |> Json.Encode.list
            (\track ->
                Json.Encode.object
                    [ ( "path", Json.Encode.string track.path )
                    , ( "sourceId", Json.Encode.string track.sourceId )
                    , ( "trackId", Json.Encode.string track.id )
                    ]
            )
        |> Alien.broadcast Alien.SyncTrackTags
        |> Ports.toBrain
        |> Return.communicate


toggleCachedOnly : Manager
toggleCachedOnly model =
    { model | cachedTracksOnly = not model.cachedTracksOnly }
        |> reviseCollection Collection.harvest
        |> andThen User.saveEnclosedUserData
        |> andThen Common.forceTracksRerender


toggleCoverSelectionReducesPool : Manager
toggleCoverSelectionReducesPool model =
    { model | coverSelectionReducesPool = not model.coverSelectionReducesPool }
        |> Queue.reset
        |> andThen User.saveSettings


toggleFavourite : Int -> Manager
toggleFavourite index model =
    case List.getAt index model.tracks.harvested of
        Just ( i, t ) ->
            let
                newFavourites =
                    Favourites.toggleInFavouritesList ( i, t ) model.favourites

                effect collection =
                    collection
                        |> Collection.map (Favourites.toggleInTracksList t)
                        |> (if model.favouritesOnly then
                                Collection.harvest

                            else
                                identity
                           )

                selectedCover =
                    Maybe.map
                        (\cover ->
                            cover.tracks
                                |> Favourites.toggleInTracksList t
                                |> (\a -> { cover | tracks = a })
                        )
                        model.selectedCover
            in
            { model | favourites = newFavourites, selectedCover = selectedCover }
                |> reviseCollection effect
                |> andThen User.saveFavourites
                |> (if model.scene == Covers then
                        andThen generateCovers >> andThen harvestCovers

                    else
                        identity
                   )

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

        arrangementChanged =
            if collectionChanged then
                True

            else
                Collection.identifiedTracksChanged
                    model.tracks.arranged
                    newCollection.arranged

        harvestChanged =
            if arrangementChanged then
                True

            else
                Collection.identifiedTracksChanged
                    model.tracks.harvested
                    newCollection.harvested

        scrollContextChanged =
            newScrollContext /= model.tracks.scrollContext

        modelWithNewCollection =
            (if model.scene == List && scrollContextChanged then
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
        whenCollectionChanges

     else if arrangementChanged then
        whenArrangementChanges

     else if harvestChanged then
        whenHarvestChanges

     else
        identity
    )
        ( modelWithNewCollection
          -----------------------------------------
          -- Command
          -----------------------------------------
        , if scrollContextChanged then
            UI.Theme.scrollTracksToTop model.scene

          else
            Cmd.none
        )


whenHarvestChanges =
    andThen harvestCovers >> andThen Queue.reset


whenArrangementChanges =
    andThen generateCovers >> whenHarvestChanges


whenCollectionChanges =
    andThen search >> andThen Common.generateDirectoryPlaylists >> whenArrangementChanges


scrollContext : Model -> String
scrollContext model =
    String.concat
        [ Maybe.withDefault "" <| model.searchTerm
        , Maybe.withDefault "" <| Maybe.map .name model.selectedPlaylist
        ]



-- 📣  ░░  USER DATA


importHypaethral : HypaethralData -> Maybe Playlist -> Manager
importHypaethral data selectedPlaylist model =
    { model
        | favourites = data.favourites
        , selectedPlaylist = selectedPlaylist
        , tracks = { emptyCollection | untouched = data.tracks }
    }
        |> reviseCollection Collection.identify
        |> andThen search
        |> (case model.searchTerm of
                Just _ ->
                    identity

                Nothing ->
                    andThen afterInitialLoad
           )



-- ㊙️


type FavouritesManagementAction
    = AddToFavourites
    | RemoveFromFavourites
