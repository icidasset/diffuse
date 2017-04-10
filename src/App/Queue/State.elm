module Queue.State exposing (..)

import Date
import List.Extra as List
import Queue.Ports as Ports
import Queue.Types as Types exposing (..)
import Queue.Utils exposing (..)
import Random
import Random.List exposing (shuffle)
import Sources.Types exposing (Source)
import Tracks.Types exposing (Track)
import Tracks.Utils
import Types as TopLevel
import Utils exposing (do)


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { activeItem = Nothing
    , future = []
    , past = []

    --
    , timestamp = Date.fromTime 0
    , tracks = Tracks.Utils.decodeTracks flags

    -- Settings
    , repeat = flags.settings.queue.repeat
    , shuffle = flags.settings.queue.shuffle
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    do TopLevel.FillQueue



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # InjectFirst Queue.Item
        -- > Add an item in front of the queue.
        --
        InjectFirst item ->
            (!)
                { model
                    | future =
                        model.future
                            |> List.filter (\i -> not (i.manualEntry == False && i.id == item.id))
                            |> (::) item
                }
                []

        -- # InjectLast Queue.Item
        -- > Add an item after the last manual entry
        --   (ie. after the last injected item).
        --
        InjectLast item ->
            (!)
                { model
                    | future =
                        model.future
                            |> List.findIndex (.manualEntry >> (==) True)
                            |> Maybe.map (\idx -> List.setAt idx item model.future)
                            |> Maybe.map (Maybe.withDefault model.future)
                            |> Maybe.withDefault model.future
                }
                []

        -- # RemoveItem Int
        -- > Remove an item from the queue.
        --
        RemoveItem index ->
            ($)
                { model
                    | future =
                        model.future
                            |> List.removeAt index
                }
                []
                [ do TopLevel.FillQueue ]

        ------------------------------------
        -- Position
        ------------------------------------
        -- # Rewind
        -- > Put the previously played item as the current one.
        --
        Rewind ->
            let
                newActiveItem =
                    List.last model.past

                newModel =
                    { model
                        | activeItem =
                            newActiveItem
                        , future =
                            model.activeItem
                                |> Maybe.map ((flip (::)) model.future)
                                |> Maybe.withDefault model.future
                        , past =
                            model.past
                                |> List.init
                                |> Maybe.withDefault []
                    }
            in
                ($)
                    newModel
                    []
                    [ Ports.activeQueueItemChanged newActiveItem
                    , do TopLevel.FillQueue
                    ]

        -- # Shift
        -- > Put the next item in the queue as the current one.
        --
        Shift ->
            let
                newActiveItem =
                    List.head model.future

                newModel =
                    { model
                        | activeItem =
                            newActiveItem
                        , future =
                            model.future
                                |> List.drop 1
                        , past =
                            model.activeItem
                                |> Maybe.map (List.singleton)
                                |> Maybe.map (List.append model.past)
                                |> Maybe.withDefault model.past
                    }
            in
                ($)
                    newModel
                    []
                    [ Ports.activeQueueItemChanged newActiveItem
                    , do TopLevel.FillQueue
                    ]

        ------------------------------------
        -- Contents
        ------------------------------------
        -- # Fill
        -- > Fill the queue with items.
        --   (TODO) Also checks if there no-longer-existing tracks in the queue.
        --   (TODO) Doesn't work properly yet for "non-shuffle" playback.
        --
        Fill sources ->
            ($)
                model
                [ Random.generate (FillStepTwo sources) (shuffle model.tracks) ]
                []

        FillStepTwo sources shuffledTracks ->
            let
                pastPaths =
                    List.map (.track >> .path) model.past

                futurePaths =
                    List.map (.track >> .path) model.future

                tracks =
                    if model.shuffle then
                        shuffledTracks
                    else
                        model.tracks

                tracksWoActive =
                    case model.activeItem of
                        Just item ->
                            List.filter (.path >> (/=) item.track.path) tracks

                        Nothing ->
                            tracks

                newFuture =
                    tracksWoActive
                        |> List.filter (\t -> List.notMember t.path pastPaths)
                        |> List.filter (\t -> List.notMember t.path futurePaths)
                        |> List.take (50 - (List.length futurePaths))
                        |> List.map (makeQueueItem False model.timestamp sources)
                        |> List.append model.future

                newFuture_ =
                    if List.length newFuture == 0 then
                        tracksWoActive
                            |> List.take 50
                            |> List.map (makeQueueItem False model.timestamp sources)
                    else
                        newFuture
            in
                (!)
                    { model | future = newFuture_ }
                    []

        -- # Reset (TODO)
        -- > Renew the queue, meaning that the auto-generated items in the queue
        --   are removed and new items are added.
        --
        Reset ->
            (!)
                model
                []

        ------------------------------------
        -- Combos
        ------------------------------------
        InjectFirstAndPlay item ->
            let
                ( a, b ) =
                    update (InjectFirst item) model

                ( c, d ) =
                    update (Shift) a
            in
                ($) c [] [ b, d ]

        ------------------------------------
        -- Settings
        ------------------------------------
        ToggleRepeat ->
            model
                |> (\m -> { m | repeat = not model.repeat })
                |> (\m -> ($) m [] [ storeSettings m ])

        ToggleShuffle ->
            model
                |> (\m -> { m | shuffle = not model.shuffle })
                |> (\m -> ($) m [ do Reset ] [ storeSettings m ])

        ------------------------------------
        -- Tracks
        ------------------------------------
        AddTracks additionalTracks ->
            let
                col =
                    additionalTracks
                        |> List.append model.tracks
                        |> List.sortBy trackSortComparable
            in
                ($)
                    { model | tracks = col }
                    []
                    [ do TopLevel.FillQueue, Tracks.Utils.storeTracks col ]

        RemoveTracks sourceId ->
            let
                col =
                    List.filter
                        (\t -> t.sourceId /= sourceId)
                        model.tracks
            in
                ($)
                    { model | tracks = col }
                    []
                    [ do TopLevel.FillQueue, Tracks.Utils.storeTracks col ]

        RemoveTracksByPaths sourceId pathsList ->
            let
                col =
                    List.filter
                        (\t ->
                            if t.sourceId == sourceId then
                                List.notMember t.path pathsList
                            else
                                True
                        )
                        model.tracks
            in
                ($)
                    { model | tracks = col }
                    []
                    [ do TopLevel.FillQueue, Tracks.Utils.storeTracks col ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.activeQueueItemEnded (\() -> Shift) ]



-- Utils


{-| Sort.
-}
trackSortComparable : Track -> String
trackSortComparable t =
    t.tags.title
        |> Maybe.withDefault ""
        |> String.append (toString (Maybe.withDefault 0 t.tags.nr))
        |> String.append (Maybe.withDefault "" t.tags.album)
        |> String.append (Maybe.withDefault "" t.tags.artist)


{-| Store settings via port.
-}
storeSettings : Model -> Cmd TopLevel.Msg
storeSettings model =
    Ports.storeQueueSettings
        { repeat = model.repeat
        , shuffle = model.shuffle
        }
