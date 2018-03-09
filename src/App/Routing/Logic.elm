module Routing.Logic exposing (locationToMessage, locationToPage, isSameBase, pageToHref)

import Http
import Navigation
import Playlists.Types as Playlists
import Queue.Types as Queue
import Routing.Types exposing (..)
import Sources.Services
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
    let
        page =
            location
                |> UrlParser.parsePath route
                |> Maybe.withDefault (ErrorScreen "Page not found.")
    in
        case page of
            Sources (Sources.NewThroughRedirect service _) ->
                location
                    |> UrlParser.parseHash string
                    |> Maybe.withDefault "unparsableHash"
                    |> Sources.NewThroughRedirect service
                    |> Sources

            _ ->
                page


{-| Base `Page`.
-}
isSameBase : Page -> Page -> Bool
isSameBase a b =
    case a of
        --
        -- Nested, Pt. 1
        --
        Playlists _ ->
            case b of
                Playlists _ ->
                    True

                Index ->
                    True

                _ ->
                    False

        Queue _ ->
            case b of
                Queue _ ->
                    True

                Index ->
                    True

                _ ->
                    False

        Sources _ ->
            case b of
                Sources _ ->
                    True

                _ ->
                    False

        --
        -- Nested, Pt. 2
        --
        Abroad ->
            case b of
                Settings ->
                    True

                _ ->
                    False

        Equalizer ->
            case b of
                Index ->
                    True

                _ ->
                    False

        --
        -- Fallback
        --
        _ ->
            a == b


{-| `Page` to `href`.
-}
pageToHref : Page -> String
pageToHref page =
    case page of
        Abroad ->
            "/import-export"

        Equalizer ->
            "/equalizer"

        Index ->
            "/"

        Playlists Playlists.Index ->
            "/playlists"

        Playlists Playlists.New ->
            "/playlists/new"

        Playlists (Playlists.Edit name) ->
            "/playlists/edit/" ++ Http.encodeUri name

        Queue Queue.Index ->
            "/queue"

        Queue Queue.History ->
            "/queue/history"

        Settings ->
            "/settings"

        Sources Sources.Index ->
            "/sources"

        Sources Sources.New ->
            "/sources/new"

        Sources (Sources.Edit sourceId) ->
            "/sources/edit/" ++ sourceId

        Sources (Sources.NewThroughRedirect service _) ->
            "/sources/new/" ++ (service |> Sources.Services.typeToKey |> String.toLower)

        ------------------------------------
        -- Screens
        ------------------------------------
        ErrorScreen _ ->
            "/"

        MessageScreen _ ->
            "/"



-- Routes


route : Parser (Page -> a) a
route =
    oneOf
        [ map (Sources Sources.Index) (s "sources")
        , map (Sources Sources.New) (s "sources" </> s "new")
        , map (Sources << Sources.Edit) (s "sources" </> s "edit" </> string)

        -- Sources ~ Services
        , map
            (Sources <| Sources.NewThroughRedirect Sources.Dropbox "")
            (s "sources" </> s "new" </> s "dropbox")

        -- Playlists
        , map (Playlists Playlists.Index) (s "playlists")
        , map (Playlists Playlists.New) (s "playlists" </> s "new")
        , map (Playlists << Playlists.Edit) (s "playlists" </> s "edit" </> string)

        -- Queue
        , map (Queue Queue.Index) (s "queue")
        , map (Queue Queue.History) (s "queue" </> s "history")

        -- Other
        , map Abroad (s "import-export")
        , map Equalizer (s "equalizer")
        , map Settings (s "settings")
        , map Index top
        ]
