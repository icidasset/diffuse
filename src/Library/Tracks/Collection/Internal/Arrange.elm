module Tracks.Collection.Internal.Arrange exposing (arrange)

import Dict
import Time
import Time.Ext as Time
import Tracks exposing (..)
import Tracks.Sorting as Sorting



-- 🍯


arrange : Parcel -> Parcel
arrange ( deps, collection ) =
    case deps.grouping of
        Just AddedOnGroups ->
            ( deps, groupByInsertedAt deps collection )

        Just TrackYearGroups ->
            -- TODO
            collection.identified
                |> Sorting.sort deps.sortBy deps.sortDirection
                |> (\x -> { collection | arranged = x })
                |> (\x -> ( deps, x ))

        Nothing ->
            collection.identified
                |> Sorting.sort deps.sortBy deps.sortDirection
                |> (\x -> { collection | arranged = x })
                |> (\x -> ( deps, x ))



-- GROUPING


groupByInsertedAt : CollectionDependencies -> Collection -> Collection
groupByInsertedAt deps collection =
    let
        groupedTracksDictionary =
            List.foldl
                (\( i, t ) ->
                    let
                        ( year, month ) =
                            ( Time.toYear Time.utc t.insertedAt
                            , Time.toMonth Time.utc t.insertedAt
                            )

                        item =
                            ( { i
                                | group =
                                    Just
                                        { name = insertedAtGroupName year month
                                        , index = 0
                                        }
                              }
                            , t
                            )
                    in
                    Dict.update
                        (year * 1000 + Time.monthNumber month)
                        (\maybeList ->
                            case maybeList of
                                Just list ->
                                    Just (item :: list)

                                Nothing ->
                                    Just [ item ]
                        )
                )
                Dict.empty
                collection.identified
    in
    groupedTracksDictionary
        |> Dict.values
        |> List.reverse
        |> List.map (Sorting.sort deps.sortBy deps.sortDirection >> List.indexedMap setIndexInGroup)
        |> List.concat
        |> (\arranged -> { collection | arranged = arranged })


insertedAtGroupName : Int -> Time.Month -> String
insertedAtGroupName year month =
    month
        |> Time.monthNumber
        |> String.fromInt
        |> String.padLeft 2 '0'
        |> (\m -> m ++ " / " ++ String.fromInt year)


setIndexInGroup : Int -> IdentifiedTrack -> IdentifiedTrack
setIndexInGroup idx ( i, t ) =
    ( { i | group = Maybe.map (\g -> { g | index = idx }) i.group }
    , t
    )
