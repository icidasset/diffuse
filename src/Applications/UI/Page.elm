module UI.Page exposing (Page(..), fromUrl, toString)

import Sources
import Url exposing (Url)
import Url.Parser exposing (..)



-- 🌳


type Page
    = Index
    | Sources Sources.Page
      --
    | NotFound



-- ⚡️


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
        -- Sources
        -----------------------------------------
        Sources Sources.Index ->
            "/sources"



-- ⚗️


route : Parser (Page -> a) a
route =
    oneOf
        [ map Index top
        , map NotFound (s "404")

        -- Sources
        , map (Sources Sources.Index) (s "sources")
        ]
