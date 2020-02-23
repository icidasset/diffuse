module UI.Alfred.State exposing (..)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import Chunky exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (autofocus, id, placeholder, type_)
import Html.Events exposing (onInput)
import Html.Ext exposing (onTapPreventDefault)
import Json.Decode
import Keyboard
import List.Extra as List
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Return exposing (andThen, return)
import Return.Ext as Return
import String.Ext as String
import Task
import UI.Alfred.Types as Alfred exposing (..)
import UI.Types as UI exposing (Manager)



-- 📣


update : Alfred.Msg UI.Msg -> Manager
update msg =
    case msg of
        Assign a ->
            assign a

        DetermineResults a ->
            determineResults a

        KeyDown a ->
            keyDown a

        RunAction a ->
            runAction a



-- 🔱


assign : Alfred UI.Msg -> Manager
assign instance model =
    return
        { model | alfred = Just instance }
        (Task.attempt
            (always UI.Bypass)
            (Dom.focus "diffuse__alfred")
        )


determineResults : String -> Manager
determineResults searchTerm model =
    model.alfred
        |> Maybe.map (determineResults_ searchTerm)
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


keyDown : Maybe Keyboard.Key -> Manager
keyDown maybeKey model =
    case maybeKey of
        Just Keyboard.ArrowDown ->
            case model.alfred of
                Just instance ->
                    instance
                        |> (\i -> { i | focus = min (i.focus + 1) (List.length i.results - 1) })
                        |> (\i -> { model | alfred = Just i })
                        |> Return.singleton

                Nothing ->
                    Return.singleton model

        Just Keyboard.ArrowUp ->
            case model.alfred of
                Just instance ->
                    instance
                        |> (\i -> { i | focus = max (i.focus - 1) 0 })
                        |> (\i -> { model | alfred = Just i })
                        |> Return.singleton

                Nothing ->
                    Return.singleton model

        Just Keyboard.Enter ->
            case model.alfred of
                Just instance ->
                    update (RunAction instance.focus) model

                Nothing ->
                    Return.singleton model

        _ ->
            Return.singleton model



-- ⚗️


determineResults_ : String -> Alfred UI.Msg -> Alfred UI.Msg
determineResults_ searchTerm alfred =
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



-- 📰


subscriptions : Sub UI.Msg
subscriptions =
    Keyboard.downs (Keyboard.anyKeyUpper >> KeyDown >> UI.Alfred)
