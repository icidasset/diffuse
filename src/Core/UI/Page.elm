module UI.Page exposing (Page(..), fromUrl, rewriteUrl, sameBase, sources, toString)

import Maybe.Extra as Maybe
import Sources exposing (Service(..))
import UI.Playlists.Page as Playlists
import UI.Queue.Page as Queue
import UI.Settings.Page as Settings
import UI.Sources.Page as Sources
import Url exposing (Url)
import Url.Ext as Url
import Url.Parser exposing (..)
import Url.Parser.Query as Query



-- 🌳


type Page
    = Index
    | Queue Queue.Page
    | Playlists Playlists.Page
    | Settings Settings.Page
    | Sources Sources.Page



-- 🔱


fromUrl : Url -> Maybe Page
fromUrl =
    parse route


rewriteUrl : Url -> Url
rewriteUrl url =
    if Maybe.unwrap False (String.contains "path=") url.query then
        -- Sometimes we have to use this kind of routing when doing redirections
        let
            maybePath =
                Url.extractQueryParam "path" url

            path =
                Maybe.withDefault "" maybePath
        in
        if
            Maybe.unwrap
                False
                (\f -> String.contains "token=" f || String.contains "code=" f)
                url.fragment
        then
            -- For some oauth stuff, replace the query with the fragment
            { url | path = path, query = url.fragment }

        else
            { url | path = path }

    else
        -- Otherwise do hash-based routing and replace the path with the fragment
        { url | path = Maybe.withDefault "" url.fragment }


toString : Page -> String
toString =
    toString_ >> (++) "#/"


toString_ : Page -> String
toString_ page =
    case page of
        Index ->
            ""

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        Playlists Playlists.Index ->
            "playlists"

        Playlists Playlists.NewCollection ->
            "playlists/new/collection"

        Playlists Playlists.NewPlaylist ->
            "playlists/new/playlist"

        Playlists (Playlists.Edit playlistName) ->
            "playlists/edit/" ++ playlistName

        -----------------------------------------
        -- Queue
        -----------------------------------------
        Queue Queue.History ->
            "queue/history"

        Queue Queue.Index ->
            "queue"

        -----------------------------------------
        -- Settings
        -----------------------------------------
        Settings Settings.Data ->
            "settings/data"

        Settings Settings.Index ->
            "settings"

        Settings Settings.Sync ->
            "settings/sync"

        -----------------------------------------
        -- Sources
        -----------------------------------------
        Sources (Sources.Edit sourceId) ->
            "sources/edit/" ++ sourceId

        Sources Sources.Index ->
            "sources"

        Sources Sources.New ->
            "sources/new"

        Sources Sources.NewOnboarding ->
            "sources/welcome"

        Sources (Sources.NewThroughRedirect Dropbox _) ->
            "sources/new/dropbox"

        Sources (Sources.NewThroughRedirect Google _) ->
            "sources/new/google"

        Sources (Sources.NewThroughRedirect _ _) ->
            "sources/new"

        Sources (Sources.Rename sourceId) ->
            "sources/rename/" ++ sourceId


{-| Are the bases of these two pages the same?
-}
sameBase : Page -> Page -> Bool
sameBase a b =
    case ( a, b ) of
        ( Playlists _, Playlists _ ) ->
            True

        ( Queue _, Queue _ ) ->
            True

        ( Settings _, Settings _ ) ->
            True

        ( Sources _, Sources _ ) ->
            True

        ( Index, Playlists _ ) ->
            True

        ( Index, Queue _ ) ->
            True

        _ ->
            a == b



-- 🔱  ░░  SPECIFIC


sources : Page -> Maybe Sources.Page
sources page =
    case page of
        Sources s ->
            Just s

        _ ->
            Nothing



-- ⚗️


route : Parser (Page -> a) a
route =
    oneOf
        [ map Index top

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        , map (Playlists Playlists.Index) (s "playlists")
        , map (Playlists << Playlists.Edit) (s "playlists" </> s "edit" </> string)
        , map (Playlists Playlists.NewCollection) (s "playlists" </> s "new" </> s "collection")
        , map (Playlists Playlists.NewPlaylist) (s "playlists" </> s "new" </> s "playlist")

        -----------------------------------------
        -- Queue
        -----------------------------------------
        , map (Queue Queue.Index) (s "queue")
        , map (Queue Queue.History) (s "queue" </> s "history")

        -----------------------------------------
        -- Settings
        -----------------------------------------
        , map (Settings Settings.Data) (s "settings" </> s "data")
        , map (Settings Settings.Sync) (s "settings" </> s "sync")
        , map (Settings Settings.Index) (s "settings")

        -----------------------------------------
        -- Sources
        -----------------------------------------
        , map (Sources Sources.Index) (s "sources")
        , map (Sources << Sources.Edit) (s "sources" </> s "edit" </> string)
        , map (Sources Sources.New) (s "sources" </> s "new")
        , map (Sources Sources.NewOnboarding) (s "sources" </> s "welcome")
        , map (Sources << Sources.Rename) (s "sources" </> s "rename" </> string)

        -- Oauth
        --------
        , map
            (\token state ->
                { codeOrToken = token, state = state }
                    |> Sources.NewThroughRedirect Dropbox
                    |> Sources
            )
            (s "sources"
                </> s "new"
                </> s "dropbox"
                <?> Query.string "access_token"
                <?> Query.string "state"
            )
        , map
            (\code state ->
                { codeOrToken = code, state = state }
                    |> Sources.NewThroughRedirect Google
                    |> Sources
            )
            (s "sources"
                </> s "new"
                </> s "google"
                <?> Query.string "code"
                <?> Query.string "state"
            )
        ]
