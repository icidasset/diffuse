module BackgroundImage.View exposing (..)

import BackgroundImage.Styles exposing (..)
import BackgroundImage.Types exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import String.Interpolate exposing (interpolate)
import Utils exposing (cssClass)


entry : Model -> Html Msg
entry model =
    div
        [ cssClass BackgroundImage
        , style [ ( "background-image", interpolate "url({0})" [ model.imageUrl ] ) ]
        ]
        []
