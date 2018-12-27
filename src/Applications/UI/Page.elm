module UI.Page exposing (Page(..), fromUrl, sameBase, toString)

import Sources
import Url exposing (Url)
import Url.Parser exposing (..)



-- 🌳


type Page
    = Index
    | Settings
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

        Settings ->
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
        , map Settings (s "settings")

        -- Sources
        , map (Sources Sources.Index) (s "sources")
        , map (Sources Sources.New) (s "sources" </> s "new")
        ]
