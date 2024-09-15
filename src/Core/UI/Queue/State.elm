module UI.Queue.State exposing (..)

import Coordinates
import Debouncer.Basic as Debouncer
import Dict
import Html.Events.Extra.Mouse as Mouse
import List.Extra as List
import Notifications
import Queue exposing (..)
import Return exposing (andThen)
import Return.Ext as Return
import Tracks exposing (..)
import UI.Audio.Types exposing (AudioLoadingState(..))
import UI.Common.State as Common
import UI.Ports as Ports
import UI.Queue.ContextMenu as Queue
import UI.Queue.Fill as Fill
import UI.Queue.Types as Queue exposing (..)
import UI.Types exposing (..)
import UI.User.State.Export exposing (..)



-- 📣


update : Queue.Msg -> Manager
update msg =
    case msg of
        Clear ->
            clear

        PreloadNext ->
            preloadNext

        Reset ->
            reset

        ResetIgnored ->
            resetIgnored

        Rewind ->
            rewind

        Shift ->
            shift

        Select a ->
            select a

        ShowFutureMenu a b c ->
            showFutureMenu a b c

        ShowFutureNavigationMenu a ->
            showFutureNavigationMenu a

        ShowHistoryMenu a b ->
            showHistoryMenu a b

        ToggleRepeat ->
            toggleRepeat

        ToggleShuffle ->
            toggleShuffle

        ------------------------------------
        -- Future
        ------------------------------------
        AddTracks a ->
            addTracks a

        InjectFirst a b ->
            injectFirst a b

        InjectLast a b ->
            injectLast a b

        InjectFirstAndPlay a ->
            injectFirstAndPlay a

        MoveItemToFirst a ->
            moveItemToFirst a

        MoveItemToLast a ->
            moveItemToLast a

        RemoveItem a ->
            removeItem a



-- 🛠


changeActiveItem : Maybe Item -> Manager
changeActiveItem maybeItem model =
    let
        maybeNowPlaying =
            Maybe.map
                (\item ->
                    { coverLoaded = False
                    , duration = Nothing
                    , isPlaying = False
                    , item = item
                    , loadingState = Loading
                    , playbackPosition = 0
                    }
                )
                maybeItem
    in
    maybeItem
        |> Maybe.map (.identifiedTrack >> Tuple.second)
        |> Maybe.map
            (Queue.makeEngineItem
                False
                model.currentTime
                model.sources
                model.cachedTracks
                (if model.rememberProgress then
                    model.progress

                 else
                    Dict.empty
                )
            )
        |> Maybe.map insertTrack
        |> Maybe.withDefault Return.singleton
        |> (\fn -> fn { model | nowPlaying = maybeNowPlaying })
        |> andThen fill


clear : Manager
clear model =
    fill { model | playingNext = [] }


fill : Manager
fill model =
    let
        ( availableTracks, timestamp ) =
            ( case ( model.selectedCover, model.coverSelectionReducesPool ) of
                ( Just cover, True ) ->
                    Tuple.first <| List.foldl coverTracksHarvester ( [], cover.trackIds ) model.tracks.harvested

                _ ->
                    model.tracks.harvested
              --
            , model.currentTime
            )

        nonMissingTracks =
            List.filter
                (Tuple.second >> .id >> (/=) Tracks.missingId)
                availableTracks
    in
    model
        |> (\m ->
                -- Empty the ignored list when we are ignoring all the tracks
                if List.length model.dontPlay == List.length nonMissingTracks then
                    { m | dontPlay = [] }

                else
                    m
           )
        |> (\m ->
                if m.shuffle && List.length model.playingNext >= Fill.queueLength then
                    m

                else
                    let
                        fillState =
                            { activeItem = Maybe.map .item m.nowPlaying
                            , future = m.playingNext
                            , ignored = m.dontPlay
                            , past = m.playedPreviously
                            }
                    in
                    -- Fill using the appropiate method
                    if m.shuffle then
                        { m | playingNext = Fill.shuffled timestamp nonMissingTracks fillState }

                    else
                        { m | playingNext = Fill.ordered timestamp nonMissingTracks fillState }
           )
        |> Return.communicate
            (Queue.PreloadNext
                |> QueueMsg
                |> Debouncer.provideInput
                |> AudioPreloadDebounce
                |> Return.task
            )


insertTrack : EngineItem -> Manager
insertTrack item model =
    item
        |> (\engineItem ->
                if
                    List.any
                        (\a -> engineItem.trackId == a.trackId)
                        model.audioElements
                then
                    List.map
                        (\a ->
                            if engineItem.trackId == a.trackId then
                                { a | isPreload = False }

                            else
                                a
                        )
                        model.audioElements

                else
                    model.audioElements ++ [ engineItem ]
           )
        |> List.filter
            (\a ->
                if item.isPreload then
                    True

                else if a.trackId /= item.trackId && not a.isPreload then
                    False

                else
                    True
            )
        |> (\a -> { model | audioElements = a })
        |> Return.singleton
        |> Return.effect_
            (\m ->
                Ports.renderAudioElements
                    { items = m.audioElements
                    , play =
                        if item.isPreload then
                            Nothing

                        else
                            Just item.trackId
                    , volume = m.eqSettings.volume
                    }
            )


preloadNext : Manager
preloadNext model =
    case List.head model.playingNext of
        Just item ->
            item
                |> .identifiedTrack
                |> Tuple.second
                |> Queue.makeEngineItem
                    True
                    model.currentTime
                    model.sources
                    model.cachedTracks
                    (if model.rememberProgress then
                        model.progress

                     else
                        Dict.empty
                    )
                |> (\engineItem ->
                        insertTrack engineItem model
                   )

        Nothing ->
            Return.singleton model


rewind : Manager
rewind model =
    changeActiveItem
        (List.last model.playedPreviously)
        { model
            | playingNext =
                model.nowPlaying
                    |> Maybe.map (\{ item } -> item :: model.playingNext)
                    |> Maybe.withDefault model.playingNext
            , playedPreviously =
                model.playedPreviously
                    |> List.init
                    |> Maybe.withDefault []
        }


{-| Renew the queue, meaning that the auto-generated items in the queue are removed and new items are added.
-}
reset : Manager
reset model =
    model.playingNext
        |> List.filter (.manualEntry >> (==) True)
        |> (\f -> { model | playingNext = f })
        |> fill


resetIgnored : Manager
resetIgnored model =
    fill { model | dontPlay = [] }


select : Item -> Manager
select item model =
    Return.singleton { model | selectedQueueItem = Just item }


shift : Manager
shift model =
    changeActiveItem
        (List.head model.playingNext)
        { model
            | playingNext =
                model.playingNext
                    |> List.drop 1
            , playedPreviously =
                model.nowPlaying
                    |> Maybe.map (.item >> List.singleton)
                    |> Maybe.map (List.append model.playedPreviously)
                    |> Maybe.withDefault model.playedPreviously
        }


showFutureMenu : Item -> { index : Int } -> Mouse.Event -> Manager
showFutureMenu item { index } mouseEvent model =
    mouseEvent.clientPos
        |> Coordinates.fromTuple
        |> Queue.futureMenu
            { cached = model.cachedTracks
            , cachingInProgress = model.cachingTracksInProgress
            , itemIndex = index
            }
            item
        |> Just
        |> (\c -> { model | contextMenu = c })
        |> Return.singleton


showFutureNavigationMenu : Mouse.Event -> Manager
showFutureNavigationMenu mouseEvent model =
    mouseEvent.clientPos
        |> Coordinates.fromTuple
        |> Queue.futureNavigationMenu { manualEntries = List.filter .manualEntry model.playingNext }
        |> Just
        |> (\c -> { model | contextMenu = c })
        |> Return.singleton


showHistoryMenu : Item -> Mouse.Event -> Manager
showHistoryMenu item mouseEvent model =
    mouseEvent.clientPos
        |> Coordinates.fromTuple
        |> Queue.historyMenu
            { cached = model.cachedTracks
            , cachingInProgress = model.cachingTracksInProgress
            }
            item
        |> Just
        |> (\c -> { model | contextMenu = c })
        |> Return.singleton


toggleRepeat : Manager
toggleRepeat model =
    saveEnclosedUserData { model | repeat = not model.repeat }


toggleShuffle : Manager
toggleShuffle model =
    { model | shuffle = not model.shuffle }
        |> reset
        |> andThen saveEnclosedUserData



-- 🛠  ░░  FUTURE


addTracks : { inFront : Bool, tracks : List IdentifiedTrack } -> Manager
addTracks { inFront, tracks } =
    (if inFront then
        injectFirst

     else
        injectLast
    )
        { showNotification = True }
        tracks


{-| Add an item in front of the queue.
-}
injectFirst : { showNotification : Bool } -> List IdentifiedTrack -> Manager
injectFirst { showNotification } identifiedTracks model =
    let
        ( items, tracks ) =
            ( List.map (makeItem True) identifiedTracks
            , List.map Tuple.second identifiedTracks
            )

        cleanedFuture =
            List.foldl
                (.id >> Fill.cleanAutoGenerated model.shuffle)
                model.playingNext
                tracks

        notification =
            case tracks of
                [ t ] ->
                    ("__" ++ t.tags.title ++ "__ will be played next")
                        |> Notifications.casual

                list ->
                    list
                        |> List.length
                        |> String.fromInt
                        |> (\s -> "__" ++ s ++ " tracks__ will be played next")
                        |> Notifications.casual
    in
    { model | playingNext = items ++ cleanedFuture }
        |> (if showNotification then
                Common.showNotification notification

            else
                Return.singleton
           )
        |> andThen fill


injectFirstAndPlay : IdentifiedTrack -> Manager
injectFirstAndPlay identifiedTrack model =
    model
        |> injectFirst { showNotification = False } [ identifiedTrack ]
        |> andThen shift


{-| Add an item after the last manual entry
(ie. after the last injected item).
-}
injectLast : { showNotification : Bool } -> List IdentifiedTrack -> Manager
injectLast { showNotification } identifiedTracks model =
    let
        ( items, tracks ) =
            ( List.map (makeItem True) identifiedTracks
            , List.map Tuple.second identifiedTracks
            )

        cleanedFuture =
            List.foldl
                (.id >> Fill.cleanAutoGenerated model.shuffle)
                model.playingNext
                tracks

        manualItems =
            cleanedFuture
                |> List.filter (.manualEntry >> (==) True)
                |> List.length

        newFuture =
            []
                ++ List.take manualItems cleanedFuture
                ++ items
                ++ List.drop manualItems cleanedFuture

        notification =
            case tracks of
                [ t ] ->
                    ("__" ++ t.tags.title ++ "__ was added to the queue")
                        |> Notifications.casual

                list ->
                    list
                        |> List.length
                        |> String.fromInt
                        |> (\s -> "__" ++ s ++ " tracks__ were added to the queue")
                        |> Notifications.casual
    in
    { model | playingNext = newFuture }
        |> (if showNotification then
                Common.showNotification notification

            else
                Return.singleton
           )
        |> andThen fill


moveItemToFirst : { index : Int } -> Manager
moveItemToFirst { index } model =
    model.playingNext
        |> moveItem { from = index, to = 0, shuffle = model.shuffle }
        |> (\f -> { model | playingNext = f })
        |> fill


moveItemToLast : { index : Int } -> Manager
moveItemToLast { index } model =
    let
        to =
            model.playingNext
                |> List.filter (.manualEntry >> (==) True)
                |> List.length
    in
    model.playingNext
        |> moveItem { from = index, to = to, shuffle = model.shuffle }
        |> (\f -> { model | playingNext = f })
        |> fill


removeItem : { index : Int, item : Item } -> Manager
removeItem { index, item } model =
    let
        newFuture =
            List.removeAt index model.playingNext

        newIgnored =
            if item.manualEntry then
                model.dontPlay

            else
                item :: model.dontPlay
    in
    fill { model | playingNext = newFuture, dontPlay = newIgnored }



-- ⚗️


coverTracksHarvester :
    IdentifiedTrack
    -> ( List IdentifiedTrack, List String )
    -> ( List IdentifiedTrack, List String )
coverTracksHarvester ( i, t ) ( acc, coverTrackIds ) =
    case List.findIndex ((==) t.id) coverTrackIds of
        Just idx ->
            ( acc ++ [ ( i, t ) ]
            , List.removeAt idx coverTrackIds
            )

        Nothing ->
            ( acc
            , coverTrackIds
            )


moveItem : { from : Int, to : Int, shuffle : Bool } -> List Item -> List Item
moveItem { from, to, shuffle } collection =
    let
        subjectItem =
            collection
                |> List.getAt from
                |> Maybe.map (\s -> { s | manualEntry = True })

        fixedTarget =
            if to > from then
                to - 1

            else
                to
    in
    collection
        |> List.removeAt from
        |> List.indexedFoldr
            (\idx existingItem acc ->
                if idx == fixedTarget then
                    case subjectItem of
                        Just itemToInsert ->
                            List.append [ itemToInsert, existingItem ] acc

                        Nothing ->
                            existingItem :: acc

                else if idx < fixedTarget then
                    { existingItem | manualEntry = True } :: acc

                else
                    existingItem :: acc
            )
            []
        |> (if shuffle then
                identity

            else
                List.filter (.manualEntry >> (==) True)
           )
