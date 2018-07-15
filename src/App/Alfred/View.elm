module Alfred.View exposing (entry)

import Alfred.Styles exposing (Styles(..))
import Alfred.Types as Types exposing (Msg(..))
import Element exposing (column, el, empty, italic, text)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input exposing (focusOnLoad, hiddenLabel, placeholder, search)
import Element.Types exposing (Node)
import Json.Decode as Json
import Styles exposing (Styles(Alfred, Zed))
import Types as TopLevel
import Variables exposing (scaled)
import Variations exposing (Variations(..))


-- 🍯


entry : Types.Alfred TopLevel.Msg -> Node
entry context =
    column
        (Alfred Container)
        [ center
        , height fill
        , width fill

        --
        , inlineStyle
            [ ( "left", "0" )
            , ( "position", "fixed" )
            , ( "top", "0" )
            , ( "z-index", "901" )
            ]
        ]
        [ ------------------------------------
          -- Message
          ------------------------------------
          el
            (Alfred Message)
            [ paddingBottom (scaled 5), paddingTop (scaled 10) ]
            (text context.message)
        , ------------------------------------
          -- Input
          ------------------------------------
          el
            Zed
            [ maxWidth (px 500)
            , onWithOptions "click" stopHerePlease (Json.succeed TopLevel.NoOp)
            , onWithOptions "tap" stopHerePlease (Json.succeed TopLevel.NoOp)
            , width fill
            ]
            (search
                (Alfred Input)
                [ paddingXY (scaled 1) (scaled 0)
                , paddingBottom (scaled -1)
                ]
                { onChange = CalculateResults >> TopLevel.AlfredMsg
                , value = ""
                , label = placeholder { text = "Type to search", label = hiddenLabel "Search" }
                , options = [ focusOnLoad ]
                }
            )

        ------------------------------------
        -- Results
        ------------------------------------
        , if List.length context.results > 0 then
            el
                Zed
                [ maxWidth (px 500)
                , paddingTop (scaled -8)
                , width fill
                ]
                (column
                    (Alfred Results)
                    [ onWithOptions "click" stopHerePlease (Json.succeed TopLevel.NoOp)
                    , onWithOptions "tap" stopHerePlease (Json.succeed TopLevel.NoOp)
                    ]
                    (List.indexedMap (resultView context.focus) context.results)
                )
          else
            empty
        ]


stopHerePlease : Element.Events.Options
stopHerePlease =
    { preventDefault = True
    , stopPropagation = True
    }



-- Result


resultView : Int -> Int -> String -> Node
resultView focus idx result =
    el
        (Alfred ResultItem)
        [ idx |> RunAction |> TopLevel.AlfredMsg |> onClick
        , paddingXY (scaled -2) (scaled -5)
        , vary Active (focus == idx)
        ]
        (text result)
