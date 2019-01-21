module Replying exposing (R3D3, Updator, andThen, do, reducto, return, updateChild)

import Return2
import Return3
import Task



-- 🌳


type alias R3D3 model msg reply =
    ( model, Cmd msg, Maybe (List reply) )


type alias Updator msg model =
    msg -> model -> ( model, Cmd msg )



-- 🔱


{-| Handle the state of a child.

    NOTE: Replies are performed from left to right.

-}
updateChild :
    (msg -> model -> ( model, Cmd msg ))
    -> (reply -> msg)
    ->
        { mapCmd : childMsg -> msg
        , mapModel : childModel -> model
        , update : childMsg -> childModel -> R3D3 childModel childMsg reply
        }
    -> { model : childModel, msg : childMsg }
    -> ( model, Cmd msg )
updateChild update translateReply context data =
    data.model
        |> context.update data.msg
        |> Return3.mapCmd context.mapCmd
        |> Return3.mapModel context.mapModel
        |> reducto update translateReply


{-| Convenience function for returning the standard ( model, Cmd msg ) tuple.
-}
return : model -> Cmd msg -> ( model, Cmd msg )
return model msg =
    ( model, msg )


{-| Chain `update` calls.
-}
andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    model
        |> fn
        |> Return2.addCmd cmd


{-| Reduce a `R3D3` to a `R2D2`.
-}
reducto : Updator msg model -> (reply -> msg) -> R3D3 model msg reply -> ( model, Cmd msg )
reducto updator translator ( model, cmd, maybeReplies ) =
    maybeReplies
        |> Maybe.withDefault []
        |> List.map translator
        |> List.foldl (andThenUpdate updator) ( model, cmd )



-- 🔱  ░░  TASKS


do : msg -> Cmd msg
do msg =
    Task.perform identity (Task.succeed msg)



-----------------------------------------
-- ㊙️
-----------------------------------------


andThenUpdate : Updator msg model -> msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThenUpdate updator msg ( model, cmd ) =
    model
        |> updator msg
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])
