module View exposing (entry)

import Html exposing (Html, div, text)
import Styles exposing (..)
import Types exposing (..)
import Utils exposing (..)


-- CHILDREN

import BackgroundImage.View as BackgroundImage
import Spinner.View as Spinner


-- ENTRY


entry : Model -> Html Msg
entry model =
    div
        []
        [ -- Loading screen
          if model.showLoadingScreen then
            div
                [ cssClass InTheMiddle ]
                [ Spinner.entry ]
          else
            text ""
        , --
          BackgroundImage.entry model.backgroundImage
            |> Html.map BackgroundImageMsg
        ]
