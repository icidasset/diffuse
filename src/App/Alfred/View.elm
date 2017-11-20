module Alfred.View exposing (entry)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit, onWithOptions)
import Json.Decode as Json
import Types as TopLevel
import Utils exposing (cssClass)


-- Styles

import Alfred.Styles exposing (Classes(..))


-- 🍯


entry : TopLevel.Alfred -> Html TopLevel.Msg
entry context =
    div
        [ cssClass Alfred
        , onWithOptions "click" eventOptions (Json.succeed TopLevel.NoOp)
        , onWithOptions "tap" eventOptions (Json.succeed TopLevel.NoOp)
        ]
        [ ------------------------------------
          -- Input
          ------------------------------------
          Html.form
            [ cssClass AlfredInput
            , onSubmit TopLevel.RunAlfredAction
            ]
            [ input
                [ type_ "text"
                , onInput TopLevel.CalculateAlfredResults
                , placeholder "Search"
                , autofocus True
                ]
                []
            ]

        ------------------------------------
        -- Message
        ------------------------------------
        , div
            [ cssClass AlfredMessage ]
            [ text context.message ]

        ------------------------------------
        -- Results
        ------------------------------------
        , ul
            [ cssClass AlfredResults ]
            (List.map resultView context.results)
        ]


eventOptions : Html.Events.Options
eventOptions =
    { preventDefault = True
    , stopPropagation = True
    }



-- Result


resultView : String -> Html TopLevel.Msg
resultView result =
    li
        []
        [ text result ]
