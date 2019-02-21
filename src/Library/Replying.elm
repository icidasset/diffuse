module Replying exposing (R3D3, Updator, addReply, andThen2, andThen3, do, reducto, return, updateChild)

import Return2
import Return3
import Task



-- 🌳


type alias R3D3 model msg reply =
    ( model, Cmd msg, Maybe (List reply) )


type alias Updator msg model =
    msg -> model -> ( model, Cmd msg )



-- 🔱


{-| Add a reply.
-}
addReply : reply -> R3D3 model msg reply -> R3D3 model msg reply
addReply reply ( model, cmd, maybeReplies ) =
    ( model
    , cmd
    , maybeReplies
        |> Maybe.map (\l -> l ++ [ reply ])
        |> Maybe.withDefault [ reply ]
        |> Just
    )


{-| Add a list of replies in front of the existing replies.
-}
addRepliesInFront : List reply -> R3D3 model msg reply -> R3D3 model msg reply
addRepliesInFront replies ( model, cmd, maybeReplies ) =
    ( model
    , cmd
    , maybeReplies
        |> Maybe.map (\l -> replies ++ l)
        |> Maybe.withDefault replies
        |> Just
    )


{-| Chain R2D2 `update` calls.
-}
andThen2 : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen2 fn ( model, cmd ) =
    Return2.addCmd cmd (fn model)


{-| Chain R3D3 `update` calls.
-}
andThen3 : (model -> R3D3 model msg reply) -> R3D3 model msg reply -> R3D3 model msg reply
andThen3 fn ( model, cmd, maybeReplies ) =
    case maybeReplies of
        Just replies ->
            Return3.addCmd cmd (fn model) |> addRepliesInFront replies

        Nothing ->
            Return3.addCmd cmd (fn model)


{-| Reduce a `R3D3` to a `R2D2`.
-}
reducto : Updator msg model -> (reply -> msg) -> R3D3 model msg reply -> ( model, Cmd msg )
reducto updator translator ( model, cmd, maybeReplies ) =
    maybeReplies
        |> Maybe.withDefault []
        |> List.map translator
        |> List.foldl (andThenUpdate updator) ( model, cmd )


{-| Convenience function for returning the standard ( model, Cmd msg ) tuple.
-}
return : model -> Cmd msg -> ( model, Cmd msg )
return model msg =
    ( model, msg )


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
