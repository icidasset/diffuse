module Queue.State exposing (..)

import List.Extra as List
import Queue.Ports as Ports
import Queue.Types as Types exposing (..)
import Queue.Utils exposing (..)
import Tracks.Utils
import Types as TopLevel


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { activeItem = Nothing
    , future = []
    , past = []
    , tracks = Tracks.Utils.decodeTracks flags

    -- Settings
    , repeat = flags.settings.queue.repeat
    , shuffle = flags.settings.queue.shuffle
    }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



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
            (!)
                { model
                    | future =
                        model.future
                            |> List.removeAt index
                }
                []

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
                newModel
                    |> fill
                    |> withCmds [ Ports.activeQueueItemChanged newActiveItem ]

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
                newModel
                    |> fill
                    |> withCmds [ Ports.activeQueueItemChanged newActiveItem ]

        ------------------------------------
        -- Contents
        ------------------------------------
        -- # Fill
        -- > Fill the queue with items.
        --   Also checks if there no-longer-existing tracks in the queue.
        --
        Fill ->
            (!)
                (fill model)
                []

        -- # Reset (TODO)
        -- > Renew the queue, meaning that the auto-generated items in the queue
        --   are removed and new items are added.
        --
        Reset ->
            (!)
                (fill model)
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
        -- > (TODO) Save this data in localStorage
        ------------------------------------
        ToggleRepeat ->
            (!) { model | repeat = not model.repeat } []

        ToggleShuffle ->
            (!) { model | shuffle = not model.shuffle } []

        ------------------------------------
        -- Tracks
        ------------------------------------
        AddTracks additionalTracks ->
            additionalTracks
                |> List.append model.tracks
                |> (\col -> { model | tracks = col })
                |> (\model -> ($) model [] [ Tracks.Utils.storeTracks model.tracks ])



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.activeQueueItemEnded (\() -> Shift) ]



-- Utils


{-| TODO: Properly implement.
-}
fill : Model -> Model
fill model =
    model


{-| Similar to `Response.withCmd`.
-}
withCmds : List (Cmd Msg) -> Model -> ( Model, Cmd TopLevel.Msg )
withCmds cmds model =
    ($) model cmds []
