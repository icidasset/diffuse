module Queue.State exposing (..)

import List.Extra as List
import Queue.Fill as Fill
import Queue.Ports as Ports
import Queue.Types as Types exposing (..)
import Queue.Utils exposing (..)
import Types as TopLevel
import Utils exposing (do)


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { activeItem = Nothing
    , future = []
    , past = []

    -- Settings
    , repeat = flags.settings.queue.repeat
    , shuffle = flags.settings.queue.shuffle
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # InjectFirst
        -- > Add an item in front of the queue.
        --
        InjectFirst track ->
            let
                item =
                    { manualEntry = True
                    , track = track
                    }
            in
                (!)
                    { model
                        | future = item :: model.future
                    }
                    []

        -- # InjectLast
        -- > Add an item after the last manual entry
        --   (ie. after the last injected item).
        --
        InjectLast track ->
            let
                item =
                    { manualEntry = True
                    , track = track
                    }

                manualItems =
                    model.future
                        |> List.filter (.manualEntry >> (==) True)
                        |> List.length
            in
                (!)
                    { model
                        | future =
                            List.concat
                                [ List.take manualItems model.future
                                , [ item ]
                                , List.drop manualItems model.future
                                ]
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
                    [ do (TopLevel.ActiveQueueItemChanged newActiveItem)
                    , do (TopLevel.FillQueue)
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
                    [ do (TopLevel.ActiveQueueItemChanged newActiveItem)
                    , do (TopLevel.FillQueue)
                    ]

        ------------------------------------
        -- Contents
        ------------------------------------
        -- # Fill
        -- > Fill the queue with items.
        --
        Fill timestamp tracks ->
            let
                newFuture =
                    case model.shuffle of
                        False ->
                            Fill.ordered model tracks

                        True ->
                            Fill.shuffled model timestamp tracks
            in
                (!)
                    { model | future = newFuture }
                    []

        -- # Clean
        -- > Remove no-longer-existing items from the queue.
        --
        Clean tracks ->
            let
                newPast =
                    List.filter (\i -> List.member i.track tracks) model.past

                newFuture =
                    List.filter (\i -> List.member i.track tracks) model.future
            in
                (!)
                    { model | future = newFuture, past = newPast }
                    [ do TopLevel.FillQueue ]

        -- # Reset
        -- > Renew the queue, meaning that the auto-generated items in the queue
        --   are removed and new items are added.
        --
        Reset ->
            let
                newFuture =
                    List.filter (.manualEntry >> (==) True) model.future
            in
                (!)
                    { model | future = newFuture }
                    [ do TopLevel.FillQueue ]

        ------------------------------------
        -- Combos
        ------------------------------------
        InjectFirstAndPlay track ->
            let
                ( a, b ) =
                    update (InjectFirst track) model

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



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.activeQueueItemEnded (\() -> Shift) ]



-- Utils


{-| Store settings via port.
-}
storeSettings : Model -> Cmd TopLevel.Msg
storeSettings model =
    Ports.storeQueueSettings
        { repeat = model.repeat
        , shuffle = model.shuffle
        }
