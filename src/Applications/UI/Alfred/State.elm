module UI.Alfred.State exposing (..)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import List.Extra as List
import Process
import Return exposing (return)
import Return.Ext as Return
import Task
import UI.Types as UI exposing (Manager)



-- 📣


assign : Alfred UI.Msg -> Manager
assign instance model =
    250
        |> Process.sleep
        |> Task.andThen (\_ -> Dom.focus "diffuse__alfred")
        |> Task.attempt (\_ -> UI.Bypass)
        |> return { model | alfred = Just instance }


gotInput : String -> Manager
gotInput searchTerm model =
    model.alfred
        |> Maybe.map (determineResults searchTerm)
        |> (\a -> Return.singleton { model | alfred = a })


runAction : Int -> Manager
runAction index model =
    case model.alfred of
        Just instance ->
            { result = List.getAt index instance.results
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
            instance
                |> (\i -> { i | focus = min (i.focus + 1) (List.length i.results - 1) })
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
                alfred.index
                    |> List.filter (String.toLower >> String.contains lowerSearchTerm)
                    |> List.sort
        }

    else
        { alfred
            | focus = 0
            , searchTerm = Nothing
            , results = alfred.index
        }
