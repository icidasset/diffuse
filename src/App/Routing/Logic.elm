module Routing.Logic exposing (locationToMessage, locationToPage, isSameBase, pageToHref)

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
        |> SetPage


{-| Parse the location and return a `Page`.
-}
locationToPage : Navigation.Location -> Page
locationToPage location =
    location
        |> UrlParser.parsePath route
        |> Maybe.withDefault (ErrorScreen "Page not found.")


{-| Base `Page`.
-}
isSameBase : Page -> Page -> Bool
isSameBase a b =
    case a of
        Queue _ ->
            case b of
                Queue _ ->
                    True

                _ ->
                    False

        Sources _ ->
            case b of
                Sources _ ->
                    True

                _ ->
                    False

        _ ->
            a == b


{-| `Page` to `href`.
-}
pageToHref : Page -> String
pageToHref page =
    case page of
        Equalizer ->
            "/equalizer"

        ErrorScreen _ ->
            "/"

        Index ->
            "/"

        Queue Queue.Index ->
            "/queue"

        Queue Queue.History ->
            "/queue/history"

        Settings ->
            "/settings"

        Sources Sources.Index ->
            "/sources"

        Sources (Sources.Edit sourceId) ->
            "/sources/edit/" ++ sourceId

        Sources Sources.New ->
            "/sources/new"



-- Routes


route : Parser (Page -> a) a
route =
    oneOf
        [ map (Sources Sources.Index) (s "sources")
        , map (\x -> Sources (Sources.Edit x)) (s "sources" </> s "edit" </> string)
        , map (Sources Sources.New) (s "sources" </> s "new")

        -- Queue
        , map (Queue Queue.Index) (s "queue")
        , map (Queue Queue.History) (s "queue" </> s "history")

        -- Other
        , map Equalizer (s "equalizer")
        , map Settings (s "settings")
        , map Index top
        ]
