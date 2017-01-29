module Main exposing (..)

import Html


main =
    Html.beginnerProgram
        { model = {}
        , view = \_ -> Html.text ""
        , update = \_ m -> m
        }
