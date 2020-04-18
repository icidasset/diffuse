module UI.Alfred.State exposing (..)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import Chunky exposing (..)
import List.Extra as List
import Material.Icons.Types exposing (Coloring(..))
import Return exposing (return)
import Return.Ext as Return
import String.Ext as String
import Task
import UI.Types as UI exposing (Manager)



-- 📣


assign : Alfred UI.Msg -> Manager
assign instance model =
    return
        { model | alfred = Just instance }
        (Task.attempt
            (always UI.Bypass)
            (Dom.focus "diffuse__alfred")
        )


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


selectNext : Manager
selectNext model =
    case model.alfred of
        Just instance ->
            instance
                |> (\i -> { i | focus = min (i.focus + 1) (List.length i.results - 1) })
                |> (\i -> { model | alfred = Just i })
                |> Return.singleton

        Nothing ->
            Return.singleton model


selectPrevious : Manager
selectPrevious model =
    case model.alfred of
        Just instance ->
            instance
                |> (\i -> { i | focus = max (i.focus - 1) 0 })
                |> (\i -> { model | alfred = Just i })
                |> Return.singleton

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
            | searchTerm =
                Just searchTerm
            , results =
                alfred.index
                    |> List.filter (String.toLower >> String.contains lowerSearchTerm)
                    |> List.sort
        }

    else
        { alfred
            | searchTerm = Nothing
            , results = alfred.index
        }
