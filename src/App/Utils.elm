module Utils exposing (..)

import Char
import Css.Helpers exposing (identifierToString)
import Dict
import Hex
import Html exposing (Attribute)
import Html.CssHelpers exposing (..)
import Http
import Regex exposing (regex, HowMany(All))
import Svg
import Svg.Attributes


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



-- URL stuff


{-| More extensive version of `encodeUri`.
Also covers the following characters: `! * ' ( )`

    @source https://github.com/ktonon/aws-sdk-elm/blob/master/src/AWS/Encode.elm

-}
encodeUri : String -> String
encodeUri x =
    x
        |> Http.encodeUri
        |> Regex.replace All
            (regex "[!*'()]")
            (\match ->
                match.match
                    |> String.toList
                    |> List.head
                    |> Maybe.map
                        (\char ->
                            char
                                |> Char.toCode
                                |> Hex.toString
                                |> String.toUpper
                                |> (++) "%"
                        )
                    |> Maybe.withDefault ""
            )


{-| Make a queryString param.
-}
makeQueryParam : ( String, String ) -> String
makeQueryParam ( a, b ) =
    encodeUri a ++ "=" ++ encodeUri b



-- Other


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
