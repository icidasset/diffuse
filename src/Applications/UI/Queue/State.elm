module UI.Queue.State exposing (..)

import Coordinates
import Dict
import Html.Events.Extra.Mouse as Mouse
import List.Extra as List
import Notifications
import Queue exposing (..)
import Return exposing (andThen, return)
import Return.Ext as Return
import Tracks exposing (..)
import UI.Common.State as Common
import UI.Ports as Ports
import UI.Queue.ContextMenu as Queue
import UI.Queue.Fill as Fill
import UI.Queue.Types as Queue exposing (..)
import UI.Types exposing (..)
import UI.User.State.Export as User exposing (..)



-- 📣


update : Queue.Msg -> Manager
update msg =
    case msg of
        Clear ->
            clear

        Reset ->
            reset

        Rewind ->
            rewind

        Shift ->
            shift

        Select a ->
            select a

        ShowFutureMenu a b c ->
            showFutureMenu a b c

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
    maybeItem
        |> Maybe.map
            (.identifiedTrack >> Tuple.second)
        |> Maybe.map
            (Queue.makeEngineItem
                model.currentTime
                model.sources
                model.cachedTracks
                (if model.rememberProgress then
                    model.progress

                 else
                    Dict.empty
                )
            )
        |> Ports.activeQueueItemChanged
        |> return { model | nowPlaying = maybeItem }
        |> andThen fill


clear : Manager
clear model =
    fill { model | playingNext = [], dontPlay = [] }


fill : Manager
fill model =
    let
        ( availableTracks, timestamp ) =
            ( model.tracks.harvested
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
                            { activeItem = m.nowPlaying
                            , future = m.playingNext
                            , ignored = m.dontPlay
                            , past = m.playedPreviously
                            }
                    in
                    -- Fill using the appropiate method
                    case m.shuffle of
                        False ->
                            { m | playingNext = Fill.ordered timestamp nonMissingTracks fillState }

                        True ->
                            { m | playingNext = Fill.shuffled timestamp nonMissingTracks fillState }
           )
        |> preloadNext


preloadNext : Manager
preloadNext model =
    case List.head model.playingNext of
        Just item ->
            item
                |> .identifiedTrack
                |> Tuple.second
                |> Queue.makeEngineItem
                    model.currentTime
                    model.sources
                    model.cachedTracks
                    (if model.rememberProgress then
                        model.progress

                     else
                        Dict.empty
                    )
                |> Ports.preloadAudio
                |> return model

        Nothing ->
            Return.singleton model


rewind : Manager
rewind model =
    changeActiveItem
        (List.last model.playedPreviously)
        { model
            | playingNext =
                model.nowPlaying
                    |> Maybe.map (\item -> item :: model.playingNext)
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
        |> (\f -> { model | playingNext = f, dontPlay = [] })
        |> fill


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
                    |> Maybe.map List.singleton
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
    { model | repeat = not model.repeat }
        |> saveEnclosedUserData
        |> Return.effect_ (.repeat >> Ports.setRepeat)


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
                        |> Notifications.success

                list ->
                    list
                        |> List.length
                        |> String.fromInt
                        |> (\s -> "__" ++ s ++ " tracks__ will be played next")
                        |> Notifications.success
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
                        |> Notifications.success

                list ->
                    list
                        |> List.length
                        |> String.fromInt
                        |> (\s -> "__" ++ s ++ " tracks__ were added to the queue")
                        |> Notifications.success
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
