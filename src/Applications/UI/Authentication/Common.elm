module UI.Authentication.Common exposing (..)

import UI.Authentication.Types exposing (..)
import User.Layer exposing (Method)



-- 🛠


extractMethod : State -> Maybe Method
extractMethod state =
    case state of
        Synced method ->
            Just method

        Syncing ->
            Nothing

        InputScreen method _ ->
            Just method

        NewEncryptionKeyScreen method _ ->
            Just method

        UpdateEncryptionKeyScreen method _ ->
            Just method

        NotSynced ->
            Nothing
