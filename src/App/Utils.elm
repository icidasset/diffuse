module Utils exposing (..)

import Css.Helpers exposing (identifierToString)
import Dict
import Html exposing (Attribute)
import Html.CssHelpers exposing (..)
import Http
import Svg
import Svg.Attributes
import Task


-- Css


cssClasses : List class -> Attribute msg
cssClasses =
    .class (withNamespace "")


cssClass : class -> Attribute msg
cssClass class =
    cssClasses [ class ]


cssSvgClass : class -> Svg.Attribute msg
cssSvgClass class =
    Svg.Attributes.class (identifierToString "" class)



-- Other


do : msg -> Cmd msg
do msg =
    Task.perform identity (Task.succeed msg)


{-| A child state `update` function that
    takes both child-level messages and top-level messages.
-}
illuminate : (a -> b) -> model -> List (Cmd a) -> List (Cmd b) -> ( model, Cmd b )
illuminate childMsgContainer model childCmds topLevelCmds =
    ( model
    , Cmd.batch
        [ Cmd.map childMsgContainer (Cmd.batch childCmds)
        , Cmd.batch topLevelCmds
        ]
    )


makeQueryParam : ( String, String ) -> String
makeQueryParam ( a, b ) =
    Http.encodeUri a ++ "=" ++ Http.encodeUri b
