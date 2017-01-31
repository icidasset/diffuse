module View exposing (entry)

import Html exposing (Html, div)
import Styles exposing (..)
import Types exposing (Model, Msg)
import Utils exposing (..)


-- Children

import BackgroundImage.View as BackgroundImage
import Spinner.View as Spinner


-- Entry


entry : Model -> Html Msg
entry model =
    div
        []
        [ div
            [ cssClass InTheMiddle ]
            [ Spinner.entry ]
        , BackgroundImage.entry model.backgroundImage
        ]
