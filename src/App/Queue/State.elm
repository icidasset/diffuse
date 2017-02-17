module Queue.State exposing (..)

import List.Extra as ListEx
import Queue.Ports as Ports
import Queue.Types as Types exposing (..)


-- 💧


initialModel : Settings -> Model
initialModel settings =
    { activeItem = Nothing
    , future = []
    , past = []
    , repeat = settings.repeat
    , shuffle = settings.shuffle
    }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- # InjectFirst Queue.Item
        -- > Add an item in front of the queue.
        --
        InjectFirst item ->
            (!)
                { model | future = item :: model.future }
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
                            |> ListEx.findIndex isManualEntry
                            |> Maybe.map (\idx -> ListEx.setAt idx item model.future)
                            |> Maybe.map (Maybe.withDefault model.future)
                            |> Maybe.withDefault model.future
                }
                []

        -- # RemoveItem Int
        -- > Remove an item from the queue.
        --
        RemoveItem index ->
            (!)
                { model | future = ListEx.removeAt index model.future }
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
                    ListEx.last model.past
            in
                (!)
                    { model
                        | activeItem =
                            newActiveItem
                        , future =
                            case model.activeItem of
                                Just item ->
                                    item :: model.future

                                Nothing ->
                                    model.future
                        , past =
                            Maybe.withDefault [] (ListEx.init model.past)
                    }
                    [ Ports.activeQueueItemChanged newActiveItem ]

        -- # Reset (TODO)
        -- > Renew the queue, meaning that the auto-generated items in the queue
        --   are removed and new items are added.
        --
        Reset ->
            (!)
                model
                []

        -- # Shift
        -- > Put the next item in the queue as the current one.
        --
        Shift ->
            let
                newActiveItem =
                    List.head model.future
            in
                (!)
                    { model
                        | activeItem = List.head model.future
                        , future = List.drop 1 model.future
                        , past =
                            case model.activeItem of
                                Just item ->
                                    model.past ++ (List.singleton item)

                                Nothing ->
                                    model.past
                    }
                    [ Ports.activeQueueItemChanged newActiveItem ]

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
                (!) c [ b, d ]

        ------------------------------------
        -- Settings
        ------------------------------------
        ToggleRepeat ->
            (!) { model | repeat = not model.repeat } []

        ToggleShuffle ->
            (!) { model | shuffle = not model.shuffle } []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.activeQueueItemEnded (\() -> Shift) ]



-- Utils


isManualEntry : Item -> Bool
isManualEntry item =
    item.manualEntry == True
