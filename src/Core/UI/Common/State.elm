module UI.Common.State exposing (..)

import Browser.Navigation as Nav
import Common exposing (..)
import ContextMenu exposing (ContextMenu)
import Debouncer.Basic as Debouncer exposing (Debouncer)
import List.Extra as List
import Monocle.Lens as Lens exposing (Lens)
import Notifications exposing (Notification)
import Return exposing (return)
import UI.Notifications
import UI.Page as Page exposing (Page)
import UI.Playlists.Directory
import UI.Syncing.Types as Syncing
import UI.Types as UI exposing (Manager, Model, Msg)
import User.Layer exposing (Method)



-- 📣


changeUrlUsingPage : Page -> Manager
changeUrlUsingPage page model =
    page
        |> Page.toString
        |> Nav.pushUrl model.navKey
        |> return model


debounce : (Model -> Debouncer Msg Msg) -> (Debouncer Msg Msg -> Model -> Model) -> (Debouncer.Msg Msg -> Msg) -> (Msg -> Model -> ( Model, Cmd Msg )) -> Debouncer.Msg Msg -> Manager
debounce getter setter debouncerMsgContainer update debouncerMsg model =
    let
        ( subModel, subCmd, emittedMsg ) =
            Debouncer.update debouncerMsg (getter model)

        mappedCmd =
            Cmd.map debouncerMsgContainer subCmd

        updatedModel =
            setter subModel model
    in
    case emittedMsg of
        Just emitted ->
            updatedModel
                |> update emitted
                |> Return.command mappedCmd

        Nothing ->
            return updatedModel mappedCmd


dismissNotification : { id : Int } -> Manager
dismissNotification options model =
    options
        |> UI.Notifications.dismiss model.notifications
        |> Return.map (\n -> { model | notifications = n })


forceTracksRerender : Manager
forceTracksRerender model =
    --
    -- TODO:
    --
    -- let
    --     containerId =
    --         case model.scene of
    --             Tracks.Covers ->
    --                 UI.Tracks.Scene.Covers.containerId
    --             Tracks.List ->
    --                 UI.Tracks.Scene.List.containerId
    -- in
    -- Browser.Dom.setViewportOf containerId 0 1
    --     |> Task.attempt (always UI.Bypass)
    --     |> return model
    --
    Return.singleton model


generateDirectoryPlaylists : Manager
generateDirectoryPlaylists model =
    let
        nonDirectoryPlaylists =
            List.filterNot
                .autoGenerated
                model.playlists

        directoryPlaylists =
            UI.Playlists.Directory.generate
                model.sources
                model.tracks.untouched
    in
    [ nonDirectoryPlaylists
    , directoryPlaylists
    ]
        |> List.concat
        |> (\c -> { model | playlists = c })
        |> Return.singleton


showContextMenuWithModel : UI.Model -> ContextMenu Msg -> ( UI.Model, Cmd UI.Msg )
showContextMenuWithModel model contextMenu =
    Return.singleton { model | contextMenu = Just contextMenu }


showNotification : Notification Msg -> Manager
showNotification notification model =
    model.notifications
        |> UI.Notifications.show notification
        |> Return.map (\n -> { model | notifications = n })


showNotificationWithModel : UI.Model -> Notification Msg -> ( UI.Model, Cmd UI.Msg )
showNotificationWithModel model notification =
    showNotification notification model


showSyncingNotification : Method -> Manager
showSyncingNotification method model =
    let
        notification =
            Notifications.stickyCasual "Syncing user data ..."

        syncing =
            Syncing.Syncing { method = method, notificationId = Notifications.id notification }
    in
    showNotification
        notification
        { model | syncing = syncing }


toggleLoadingScreen : Switch -> Manager
toggleLoadingScreen switch model =
    case switch of
        On ->
            Return.singleton { model | isLoading = True }

        Off ->
            Return.singleton { model | isLoading = False }



-- 🛠


modifySingleton : Lens a b -> (b -> b) -> a -> ( a, Cmd msg )
modifySingleton lens modifier =
    Lens.modify lens modifier >> Return.singleton