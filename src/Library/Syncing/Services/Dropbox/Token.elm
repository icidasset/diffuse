module Syncing.Services.Dropbox.Token exposing (..)

import Time


isExpired : { currentTime : Time.Posix, expiresAt : Int } -> Bool
isExpired { currentTime, expiresAt } =
    let
        currentTimeInSeconds =
            Time.posixToMillis currentTime // 1000

        currentTimeWithOffset =
            -- We add 60 seconds here because we only get the current time every minute,
            -- so there's always the chance the "current time" is 1-60 seconds behind.
            currentTimeInSeconds + 60
    in
    -- If the access token is expired
    currentTimeWithOffset >= expiresAt
