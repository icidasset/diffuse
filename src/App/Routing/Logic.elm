module Routing.Logic exposing (locationToMessage, locationToPage)

import Navigation
import Routing.Types exposing (..)
import UrlParser exposing (..)


{-| Parse the location and return a `Msg`.
-}
locationToMessage : Navigation.Location -> Msg
locationToMessage location =
    location
        |> locationToPage
        |> SetPage


{-| Parse the location and return a `Page`.
-}
locationToPage : Navigation.Location -> Page
locationToPage location =
    location
        |> UrlParser.parsePath route
        |> Maybe.withDefault (ErrorScreen "Page not found.")



-- Private


route : Parser (Page -> a) a
route =
    oneOf
        [ map Index top ]
