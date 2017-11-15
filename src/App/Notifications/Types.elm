module Notifications.Types exposing (..)


type Notification
    = Error String
    | Message String
    | Success String
