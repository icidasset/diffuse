module UI.Alfred.State exposing (..)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import Keyboard
import Process
import Return exposing (return)
import Return.Ext as Return
import Task
import UI.Types as UI exposing (Manager)



-- 📣


assign : Alfred UI.Msg -> Manager
assign instance model =
    let
        pressedKeys =
            List.filter
                (\k ->
                    case k of
                        Keyboard.Meta ->
                            True

                        Keyboard.Control ->
                            True

                        _ ->
                            False
                )
                model.pressedKeys
    in
    250
        |> Process.sleep
        |> Task.andThen (\_ -> Dom.focus "diffuse__alfred")
        |> Task.andThen (\_ -> Dom.setViewportOf "alfred__results" 0 0)
        |> Task.attempt (\_ -> UI.Bypass)
        -- The "K" key seems to stick when using CMD + K,
        -- aka. Meta key + K, to show the command palette.
        -- https://github.com/ohanhi/keyboard/issues/14
        |> return { model | alfred = Just instance, pressedKeys = pressedKeys }


gotInput : String -> Manager
gotInput searchTerm model =
    model.alfred
        |> Maybe.map (determineResults searchTerm)
        |> (\a -> Return.singleton { model | alfred = a })


runAction : Int -> Manager
runAction index model =
    case model.alfred of
        Just instance ->
            { result = Alfred.getAt index instance
            , searchTerm = instance.searchTerm
            }
                |> instance.action
                |> List.map Return.task
                |> Cmd.batch
                |> return { model | alfred = Nothing }

        Nothing ->
            Return.singleton { model | alfred = Nothing }


runSelectedAction : Manager
runSelectedAction model =
    case model.alfred of
        Just instance ->
            runAction instance.focus model

        Nothing ->
            Return.singleton model


scrollToFocus : Manager
scrollToFocus model =
    let
        task =
            Task.map3
                (\innerE outerE outerV ->
                    outerV.viewport.y + innerE.element.y - outerE.element.y - 9
                )
                (Dom.getElement "alfred__results__focus")
                (Dom.getElement "alfred__results")
                (Dom.getViewportOf "alfred__results")
    in
    task
        |> Task.andThen (\a -> Dom.setViewportOf "alfred__results" 0 a)
        |> Task.attempt (\_ -> UI.Bypass)
        |> return model


selectNext : Manager
selectNext model =
    case model.alfred of
        Just instance ->
            let
                total =
                    Alfred.length instance
            in
            instance
                |> (\i -> { i | focus = min (i.focus + 1) (total - 1) })
                |> (\i -> { model | alfred = Just i })
                |> scrollToFocus

        Nothing ->
            Return.singleton model


selectPrevious : Manager
selectPrevious model =
    case model.alfred of
        Just instance ->
            instance
                |> (\i -> { i | focus = max (i.focus - 1) 0 })
                |> (\i -> { model | alfred = Just i })
                |> scrollToFocus

        Nothing ->
            Return.singleton model



-- ⚗️


determineResults : String -> Alfred UI.Msg -> Alfred UI.Msg
determineResults searchTerm alfred =
    let
        lowerSearchTerm =
            searchTerm
                |> String.toLower
                |> String.trim
    in
    if String.length lowerSearchTerm > 0 then
        { alfred
            | focus = 0
            , searchTerm =
                Just searchTerm
            , results =
                List.map
                    (\group ->
                        group.items
                            |> List.filter
                                (.title >> String.toLower >> String.contains lowerSearchTerm)
                            |> (\items ->
                                    { group | items = items }
                               )
                    )
                    alfred.index
        }

    else
        { alfred
            | focus = 0
            , searchTerm = Nothing
            , results = alfred.index
        }
