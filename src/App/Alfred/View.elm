module Alfred.View exposing (entry)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Html.Events.Extra exposing (onClickPreventDefaultAndStopPropagation)
import Json.Decode
import Traits exposing (grs)
import Types as TopLevel
import Utils exposing (cssClass)


-- Styles

import Alfred.Styles exposing (Classes(..))


-- 🍯


entry : TopLevel.Alfred -> Html TopLevel.Msg
entry context =
    div
        [ cssClass Alfred
        , onClickPreventDefaultAndStopPropagation TopLevel.NoOp
        , onWithOptions "tap" tapOptions (Json.Decode.succeed TopLevel.NoOp)
        ]
        [ ------------------------------------
          -- Input
          ------------------------------------
          Html.form
            [ cssClass AlfredInput
            , onSubmit (TopLevel.RunAlfredAction context.focus)
            ]
            [ input
                [ type_ "text"
                , onInput TopLevel.CalculateAlfredResults
                , placeholder "Type to search"
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
        , if List.length context.results > 0 then
            ul
                [ cssClass AlfredResults ]
                (List.indexedMap (resultView context.focus) context.results)
          else
            text ""
        ]


tapOptions : Html.Events.Options
tapOptions =
    { preventDefault = True
    , stopPropagation = True
    }



-- Result


resultView : Int -> Int -> String -> Html TopLevel.Msg
resultView focus idx result =
    li
        [ idx |> TopLevel.RunAlfredAction |> onClick
        , style
            (if focus == idx then
                [ ( "font-weight", "bold" )
                , ( "padding-left", grs 4 )
                ]
             else
                []
            )
        ]
        [ text result ]
