module Alfred.State exposing (..)

import Alfred.System
import Alfred.Types exposing (..)
import Keyboard.Extra as Keyboard
import Response
import Response.Ext exposing (do)
import Types as TopLevel


-- 💧


initialModel : Model TopLevel.Msg
initialModel =
    { instance = Nothing
    }



-- 🔥


update : Msg TopLevel.Msg -> Model TopLevel.Msg -> ( Model TopLevel.Msg, Cmd TopLevel.Msg )
update msg model =
    case msg of
        Assign instance ->
            (!) { model | instance = Just instance } []

        CalculateResults searchTerm ->
            model.instance
                |> Maybe.map (Alfred.System.calculateResults searchTerm)
                |> (\a -> { model | instance = a })
                |> (\m -> ( m, Cmd.none ))

        Hide ->
            (!) { model | instance = Nothing } []

        RunAction index ->
            model.instance
                |> Maybe.map (Alfred.System.runAction index)
                |> Maybe.map (Tuple.mapFirst <| \a -> { model | instance = Just a })
                |> Maybe.withDefault ( model, Cmd.none )

        ------------------------------------
        -- Keyboard (Down)
        ------------------------------------
        KeydownMsg Keyboard.ArrowDown ->
            case model.instance of
                Just context ->
                    context
                        |> (\c -> { c | focus = min (List.length c.results - 1) (c.focus + 1) })
                        |> (\c -> { model | instance = Just c })
                        |> Response.withCmd Cmd.none

                Nothing ->
                    (,) model Cmd.none

        KeydownMsg Keyboard.ArrowUp ->
            case model.instance of
                Just context ->
                    context
                        |> (\c -> { c | focus = max 0 (c.focus - 1) })
                        |> (\c -> { model | instance = Just c })
                        |> Response.withCmd Cmd.none

                Nothing ->
                    (,) model Cmd.none

        KeydownMsg Keyboard.Enter ->
            case model.instance of
                Just context ->
                    (,) model (do <| TopLevel.AlfredMsg <| RunAction context.focus)

                Nothing ->
                    (,) model Cmd.none

        KeydownMsg _ ->
            (,) model Cmd.none



-- 🌱


subscriptions : Model msg -> Sub (Msg msg)
subscriptions _ =
    Sub.batch
        [ Keyboard.downs KeydownMsg
        ]
