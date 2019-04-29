module UI.Page exposing (Page(..), fromUrl, sameBase, sources, toString)

import Sources exposing (Service(..))
import UI.Queue.Page as Queue
import UI.Settings.Page as Settings
import UI.Sources.Page as Sources
import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Query



-- 🌳


type Page
    = Equalizer
    | Index
    | Queue Queue.Page
    | Settings Settings.Page
    | Sources Sources.Page



-- 🔱


fromUrl : Url -> Maybe Page
fromUrl url =
    -- For some oauth stuff, replace the query with the fragment
    if Maybe.map (String.contains "token=") url.fragment == Just True then
        parse route { url | query = url.fragment }

    else
        parse route url


toString : Page -> String
toString page =
    case page of
        Equalizer ->
            "/equalizer"

        Index ->
            "/"

        -----------------------------------------
        -- Queue
        -----------------------------------------
        Queue Queue.History ->
            "/queue/history"

        Queue Queue.Index ->
            "/queue"

        -----------------------------------------
        -- Settings
        -----------------------------------------
        Settings Settings.ImportExport ->
            "/settings/import-export"

        Settings Settings.Index ->
            "/settings"

        -----------------------------------------
        -- Sources
        -----------------------------------------
        Sources (Sources.Edit sourceId) ->
            "/sources/edit/" ++ sourceId

        Sources Sources.Index ->
            "/sources"

        Sources Sources.New ->
            "/sources/new"

        Sources (Sources.NewThroughRedirect Google _) ->
            "/sources/new/google"

        Sources (Sources.NewThroughRedirect _ _) ->
            "/sources/new"


{-| Are the bases of these two pages the same?
-}
sameBase : Page -> Page -> Bool
sameBase a b =
    case ( a, b ) of
        ( Queue _, Queue _ ) ->
            True

        ( Settings _, Settings _ ) ->
            True

        ( Sources _, Sources _ ) ->
            True

        ( Index, Equalizer ) ->
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

        --
        , map Equalizer (s "equalizer")

        -----------------------------------------
        -- Queue
        -----------------------------------------
        , map (Queue Queue.Index) (s "queue")
        , map (Queue Queue.History) (s "queue" </> s "history")

        -----------------------------------------
        -- Settings
        -----------------------------------------
        , map (Settings Settings.Index) (s "settings")
        , map (Settings Settings.ImportExport) (s "settings" </> s "import-export")

        -----------------------------------------
        -- Sources
        -----------------------------------------
        , map (Sources Sources.Index) (s "sources")
        , map (Sources << Sources.Edit) (s "sources" </> s "edit" </> string)
        , map (Sources Sources.New) (s "sources" </> s "new")

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
