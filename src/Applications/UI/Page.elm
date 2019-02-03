module UI.Page exposing (Page(..), fromUrl, sameBase, toString)

import UI.Settings.Page as Settings
import UI.Sources.Page as Sources
import Url exposing (Url)
import Url.Parser exposing (..)



-- 🌳


type Page
    = Index
    | Settings Settings.Page
    | Sources Sources.Page
      --
    | NotFound



-- 🔱


fromUrl : Url -> Page
fromUrl url =
    url
        |> parse route
        |> Maybe.withDefault NotFound


toString : Page -> String
toString page =
    case page of
        Index ->
            "/"

        NotFound ->
            "/404"

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
        Sources Sources.Index ->
            "/sources"

        Sources Sources.New ->
            "/sources/new"


{-| Are the bases of these two pages the same?
-}
sameBase : Page -> Page -> Bool
sameBase a b =
    case ( a, b ) of
        ( Settings _, Settings _ ) ->
            True

        ( Sources _, Sources _ ) ->
            True

        _ ->
            a == b



-- ⚗️


route : Parser (Page -> a) a
route =
    oneOf
        [ map Index top
        , map NotFound (s "404")

        -----------------------------------------
        -- Settings
        -----------------------------------------
        , map (Settings Settings.ImportExport) (s "settings" </> s "import-export")
        , map (Settings Settings.Index) (s "settings")

        -----------------------------------------
        -- Sources
        -----------------------------------------
        , map (Sources Sources.Index) (s "sources")
        , map (Sources Sources.New) (s "sources" </> s "new")
        ]
