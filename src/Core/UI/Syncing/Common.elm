module UI.Syncing.Common exposing (..)

import Chunky exposing (..)
import Html
import Svg
import UI.Svg.Elements
import UI.Syncing.Types exposing (..)
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method, dropboxMethod, remoteStorageMethod)



-- 🚀


startDropbox : Msg
startDropbox =
    SyncingMsg (TriggerExternalAuth dropboxMethod "")


startIpfs : Msg
startIpfs =
    SyncingMsg PingIpfs


startRemoteStorage : Msg
startRemoteStorage =
    { icon = \size _ -> Svg.map never (UI.Svg.Elements.remoteStorageLogo size)
    , placeholder = "example@5apps.com"
    , question =
        { question =
            "What's your user address?"
        , info =
            [ Html.text "The format is "
            , inline
                []
                [ Html.text "username@server.domain" ]
            ]
        }
    , value = ""
    }
        |> AskForInput remoteStorageMethod
        |> SyncingMsg



-- 🛠


extractMethod : State -> Maybe Method
extractMethod state =
    case state of
        Synced method ->
            Just method

        Syncing { method } ->
            Just method

        InputScreen method _ ->
            Just method

        NewEncryptionKeyScreen method _ ->
            Just method

        UpdateEncryptionKeyScreen method _ ->
            Just method

        NotSynced ->
            Nothing
