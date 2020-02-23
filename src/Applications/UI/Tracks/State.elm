module UI.Tracks.State exposing (..)

import ContextMenu
import List.Ext as List
import List.Extra as List
import Monocle.Lens as Lens exposing (Lens)
import Notifications
import Return exposing (andThen, return)
import Return.Ext as Return exposing (communicate)
import UI.Common.State as Common exposing (showNotification)
import UI.Reply as Reply exposing (Reply(..))
import UI.Tracks as Tracks
import UI.Types as UI exposing (Manager, Msg(..))



-- 🌳


lens =
    { get = .tracks
    , set = \tracks ui -> { ui | tracks = tracks }
    }



-- 🔱


downloadTracksFinished : Manager
downloadTracksFinished model =
    case model.downloading of
        Just { notificationId } ->
            { id = notificationId }
                |> DismissNotification
                |> Reply
                |> Return.performanceF { model | downloading = Nothing }

        Nothing ->
            Return.singleton model


failedToStoreTracksInCache : List String -> Manager
failedToStoreTracksInCache trackIds model =
    model
        |> Lens.modify lens
            (\m -> { m | cachingInProgress = List.without trackIds m.cachingInProgress })
        |> showNotification
            (Notifications.error "Failed to store track in cache")


finishedStoringTracksInCache : List String -> Manager
finishedStoringTracksInCache trackIds model =
    model
        |> Lens.modify lens
            (\t ->
                { t
                    | cached = t.cached ++ trackIds
                    , cachingInProgress = List.without trackIds t.cachingInProgress
                }
            )
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
                            m.tracks.collection.harvested
                                |> List.pickIndexes m.tracks.selectedTrackIndexes
                                |> ShowTracksContextMenu coordinates { alt = False }
                                |> Reply
                                |> Return.performanceF m

                        else
                            Return.singleton m

                    Nothing ->
                        Return.singleton m
           )
        -- TODO: Make sync
        |> Return.effect_ (\_ -> Return.task <| TracksMsg Tracks.Harvest)
        |> Return.effect_ (\_ -> Return.task <| Reply Reply.SaveEnclosedUserData)
