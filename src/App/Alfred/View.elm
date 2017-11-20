module Alfred.View exposing (entry)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
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
            , onSubmit (TopLevel.RunAlfredAction 0)
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
            (List.indexedMap resultView context.results)
        ]


eventOptions : Html.Events.Options
eventOptions =
    { preventDefault = True
    , stopPropagation = True
    }



-- Result


resultView : Int -> String -> Html TopLevel.Msg
resultView idx result =
    li
        [ idx |> TopLevel.RunAlfredAction |> onClick ]
        [ text result ]
