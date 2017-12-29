module Utils exposing (..)

import Char
import Hex
import Http
import Notifications.Types exposing (Notification(..))
import Regex exposing (regex, HowMany(All))
import Response.Ext exposing (do)
import Routing.Types exposing (Page(..))
import String.Extra
import Types as TopLevel


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



-- Display


displayError : String -> Cmd TopLevel.Msg
displayError error =
    error
        |> Error
        |> TopLevel.ShowNotification
        |> do


displayMessage : String -> Cmd TopLevel.Msg
displayMessage message =
    message
        |> MessageScreen
        |> Routing.Types.SetPage
        |> TopLevel.RoutingMsg
        |> do



-- Translations


messageToString : a -> String
messageToString msg =
    msg
        |> toString
        |> String.Extra.underscored
        |> String.toUpper



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
