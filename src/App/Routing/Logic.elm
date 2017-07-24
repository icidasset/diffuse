module Routing.Logic exposing (locationToMessage, locationToPage, pageToParentHref)

import Navigation
import Queue.Types as Queue
import Routing.Types exposing (..)
import Sources.Types as Sources
import UrlParser exposing (..)


{-| Parse the location and return a `Msg`.
-}
locationToMessage : Navigation.Location -> Msg
locationToMessage location =
    location
        |> locationToPage
        |> GoToPage


{-| Parse the location and return a `Page`.
-}
locationToPage : Navigation.Location -> Page
locationToPage location =
    location
        |> UrlParser.parsePath route
        |> Maybe.withDefault (ErrorScreen "Page not found.")


{-| Href to `Page`.
-}
pageToParentHref : Page -> String
pageToParentHref page =
    case page of
        About ->
            "/about"

        Equalizer ->
            "/eq"

        Queue _ ->
            "/queue"

        Settings ->
            "/settings"

        Sources _ ->
            "/sources"

        _ ->
            "/"



-- Routes


route : Parser (Page -> a) a
route =
    oneOf
        [ map (Sources Sources.Index) (s "sources")
        , map (Sources Sources.New) (s "sources" </> s "new")

        -- Queue
        , map (Queue Queue.Index) (s "queue")
        , map (Queue Queue.History) (s "queue" </> s "history")

        -- Other
        , map About (s "about")
        , map Equalizer (s "eq")
        , map Settings (s "settings")
        , map Index top
        ]
