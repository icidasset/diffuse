module Settings.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Material.Icons.Action
import Navigation.View as Navigation
import Types exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colorDerivatives)


-- Styles

import Styles exposing (Classes(..))


-- 🍯


entry : Model -> Html Msg
entry model =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            [ ( span
                    []
                    [ Material.Icons.Action.exit_to_app colorDerivatives.text 16
                    , label [] [ text "Sign out" ]
                    ]
              , SignOut
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Settings" ]
            , p
                [ cssClass Intro ]
                [ text """
                    Not much to see here yet.
                  """
                ]
            ]
        ]
