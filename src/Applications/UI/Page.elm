module UI.Page exposing (Page(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser exposing (..)



-- 🌳


type Page
    = Index
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



-- ⚗️


route : Parser (Page -> a) a
route =
    oneOf
        [ map Index top
        ]
