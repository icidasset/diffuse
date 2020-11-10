module UI.Authentication.Common exposing (..)

import UI.Authentication.Types exposing (..)
import User.Layer exposing (Method)



-- 🛠


isAuthenticated : State -> Bool
isAuthenticated state =
    case state of
        Authenticated _ ->
            True

        _ ->
            False


extractMethod : State -> Maybe Method
extractMethod state =
    case state of
        Authenticated method ->
            Just method

        InputScreen method _ ->
            Just method

        NewEncryptionKeyScreen method _ ->
            Just method

        UpdateEncryptionKeyScreen method _ ->
            Just method

        Unauthenticated ->
            Nothing

        Welcome ->
            Nothing
