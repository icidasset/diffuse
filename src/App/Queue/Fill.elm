module Queue.Fill exposing (ordered, shuffled, queueLength)

{-| These functions will return a new list for the `future` property.
-}

import Date exposing (Date)
import List.Extra as List
import Queue.Types exposing (Item, Model)
import Queue.Utils exposing (makeItem)
import Maybe.Ext as Maybe
import Maybe.Extra as Maybe
import Random exposing (Generator, Seed)
import Tracks.Types exposing (Track)


-- ORDERED


ordered : Model -> List Track -> List Item
ordered model tracks =
    let
        manualEntries =
            List.filter (.manualEntry >> (==) True) model.future

        remaining =
            max (queueLength - (List.length manualEntries)) 0

        focus =
            Maybe.preferFirst (List.last manualEntries) model.activeItem
    in
        case focus of
            Just item ->
                tracks
                    |> List.findIndex ((==) item.track)
                    |> Maybe.map (\idx -> List.drop (idx + 1) tracks)
                    |> Maybe.withDefault tracks
                    |> List.take remaining
                    |> (\a -> a ++ List.take (remaining - List.length a) tracks)
                    |> List.map (makeItem False)

            Nothing ->
                tracks
                    |> List.take remaining
                    |> List.map (makeItem False)



-- SHUFFLED


shuffled : Model -> Date -> List Track -> List Item
shuffled model timestamp tracks =
    let
        amountOfTracks =
            List.length tracks

        generator =
            Random.int 0 (amountOfTracks - 1)

        pastIds =
            List.map (.track >> .id) model.past

        futureIds =
            List.map (.track >> .id) model.future

        activeId =
            Maybe.map (.track >> .id) model.activeItem

        usedIndexes =
            collectIndexes
                tracks
                [ (\t -> List.member t.id pastIds)
                , (\t -> List.member t.id futureIds)
                , (\t -> Just t.id == activeId)
                ]

        usedIndexes_ =
            let
                isUsedUp =
                    List.length usedIndexes >= amountOfTracks

                hasNoFuture =
                    List.length model.future < 1
            in
                if isUsedUp && hasNoFuture && amountOfTracks > 1 then
                    case amountOfTracks > 1 of
                        True ->
                            collectIndexes tracks [ (\t -> Just t.id == activeId) ]

                        False ->
                            []
                else
                    usedIndexes

        toAmount =
            max (queueLength - (List.length model.future)) 0

        maxAmount =
            max (amountOfTracks - (List.length usedIndexes_)) 0

        howMany =
            min toAmount maxAmount
    in
        if howMany > 0 then
            timestamp
                |> Date.toTime
                |> round
                |> Random.initialSeed
                |> generateIndexes generator howMany usedIndexes_ []
                |> List.map (\idx -> List.getAt idx tracks)
                |> Maybe.values
                |> List.map (makeItem False)
                |> List.append model.future
        else
            model.future


collectIndexes : List Track -> List (Track -> Bool) -> List Int
collectIndexes tracks audits =
    List.indexedFoldl (collector audits) [] tracks


collector : List (Track -> Bool) -> Int -> Track -> List Int -> List Int
collector audits idx track acc =
    case List.foldl (auditor track) False audits of
        True ->
            idx :: acc

        False ->
            acc


auditor : Track -> (Track -> Bool) -> Bool -> Bool
auditor track audit acc =
    if acc == True then
        acc
    else
        audit track


{-| Generated random indexes.

    `squirrel` = accumulator, ie. collected indexes
-}
generateIndexes : Generator Int -> Int -> List Int -> List Int -> Seed -> List Int
generateIndexes generator howMany usedIndexes squirrel seed =
    let
        ( index, newSeed ) =
            Random.step generator seed

        newSquirrel =
            if List.member index usedIndexes then
                squirrel
            else if List.member index squirrel then
                squirrel
            else
                index :: squirrel
    in
        if List.length newSquirrel < howMany then
            generateIndexes generator howMany usedIndexes newSquirrel newSeed
        else
            newSquirrel



-- Constants


queueLength : Int
queueLength =
    30
