module UI.Interface.State exposing (..)

import Common exposing (Switch(..))
import Debouncer.Basic as Debouncer
import Notifications
import Return exposing (return)
import Return.Ext as Return
import UI.DnD as DnD
import UI.Page as Page
import UI.Playlists.State as Playlists
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Tracks.Types as Tracks
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 🔱


blur : Manager
blur model =
    Return.singleton { model | focusedOnInput = False }


contextMenuConfirmation : String -> Msg -> Manager
contextMenuConfirmation conf msg model =
    return
        { model | confirmation = Just conf }
        (Return.task msg)


copyToClipboard : String -> Manager
copyToClipboard string =
    string
        |> Ports.copyToClipboard
        |> Return.communicate


debounce : (Msg -> Model -> ( Model, Cmd Msg )) -> Debouncer.Msg Msg -> Manager
debounce update debouncerMsg model =
    let
        ( subModel, subCmd, emittedMsg ) =
            Debouncer.update debouncerMsg model.debounce

        mappedCmd =
            Cmd.map Debounce subCmd

        updatedModel =
            { model | debounce = subModel }
    in
    case emittedMsg of
        Just emitted ->
            updatedModel
                |> update emitted
                |> Return.command mappedCmd

        Nothing ->
            return updatedModel mappedCmd


dnd : DnD.Msg Int -> Manager
dnd dragMsg model =
    let
        ( d, { initiated } ) =
            DnD.update dragMsg model.dnd

        m =
            if initiated then
                { model | dnd = d, isDragging = True }

            else
                { model | dnd = d }
    in
    if DnD.hasDropped d then
        case model.page of
            Page.Queue _ ->
                let
                    ( from, to ) =
                        ( Maybe.withDefault 0 <| DnD.modelSubject d
                        , Maybe.withDefault 0 <| DnD.modelTarget d
                        )

                    newFuture =
                        Queue.moveItem
                            { from = from, to = to, shuffle = model.shuffle }
                            model.playingNext
                in
                Queue.fill { m | playingNext = newFuture }

            Page.Index ->
                case model.scene of
                    Tracks.List ->
                        Playlists.moveTrackInSelected
                            { to = Maybe.withDefault 0 (DnD.modelTarget d) }
                            m

            _ ->
                Return.singleton m

    else
        Return.singleton m


focusedOnInput : Manager
focusedOnInput model =
    Return.singleton { model | focusedOnInput = True }


hideOverlay : Manager
hideOverlay model =
    Return.singleton
        { model
            | alfred = Nothing
            , confirmation = Nothing
            , contextMenu = Nothing
        }


preferredColorSchemaChanged : { dark : Bool } -> Manager
preferredColorSchemaChanged { dark } model =
    Return.singleton { model | darkMode = dark }


msgViaContextMenu : Msg -> Manager
msgViaContextMenu msg model =
    return
        (case msg of
            ContextMenuConfirmation _ _ ->
                model

            _ ->
                { model | confirmation = Nothing, contextMenu = Nothing }
        )
        (Return.task msg)


removeNotification : { id : Int } -> Manager
removeNotification { id } model =
    model.notifications
        |> List.filter (Notifications.id >> (/=) id)
        |> (\n -> { model | notifications = n })
        |> Return.singleton


removeQueueSelection : Manager
removeQueueSelection model =
    Return.singleton { model | selectedQueueItem = Nothing }


removeTrackSelection : Manager
removeTrackSelection model =
    Return.singleton { model | selectedTrackIndexes = [] }


resizedWindow : ( Int, Int ) -> Manager
resizedWindow ( width, height ) model =
    Return.singleton
        { model
            | contextMenu = Nothing
            , viewport = { height = toFloat height, width = toFloat width }
        }


setIsTouchDevice : Bool -> Manager
setIsTouchDevice bool model =
    Return.singleton { model | isTouchDevice = bool }


stoppedDragging : Manager
stoppedDragging model =
    let
        notDragging =
            { model | isDragging = False }
    in
    -- Depending on where we stopped dragging something,
    -- do the appropriate thing.
    case model.page of
        Page.Queue _ ->
            dnd DnD.stoppedDragging notDragging

        Page.Index ->
            dnd DnD.stoppedDragging notDragging

        _ ->
            Return.singleton notDragging



-- MESSAGES


onResize : Int -> Int -> Msg
onResize w h =
    ( w, h )
        |> ResizedWindow
        |> Debouncer.provideInput
        |> Debounce
