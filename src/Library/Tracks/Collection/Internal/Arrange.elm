module Tracks.Collection.Internal.Arrange exposing (arrange)

import Dict exposing (Dict)
import Maybe.Extra as Maybe
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
            ( deps, groupByYear deps collection )

        Nothing ->
            collection.identified
                |> Sorting.sort deps.sortBy deps.sortDirection
                |> (\x -> { collection | arranged = x })
                |> (\x -> ( deps, x ))



-- GROUPING


setIndexInGroup : Int -> IdentifiedTrack -> IdentifiedTrack
setIndexInGroup idx ( i, t ) =
    ( { i | group = Maybe.map (\g -> { g | index = idx }) i.group }
    , t
    )



-- GROUPING  ░░  INSERTED AT


groupByInsertedAt : CollectionDependencies -> Collection -> Collection
groupByInsertedAt deps collection =
    collection.identified
        |> List.foldl groupByInsertedAt_ Dict.empty
        |> Dict.values
        |> List.reverse
        |> List.concatMap (Sorting.sort deps.sortBy deps.sortDirection >> List.indexedMap setIndexInGroup)
        |> (\arranged -> { collection | arranged = arranged })


groupByInsertedAt_ : IdentifiedTrack -> Dict Int (List IdentifiedTrack) -> Dict Int (List IdentifiedTrack)
groupByInsertedAt_ ( i, t ) =
    let
        ( year, month ) =
            ( Time.toYear Time.utc t.insertedAt
            , Time.toMonth Time.utc t.insertedAt
            )

        group =
            { name = insertedAtGroupName year month
            , index = 0
            }

        item =
            ( { i | group = Just group }
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


insertedAtGroupName : Int -> Time.Month -> String
insertedAtGroupName year month =
    month
        |> Time.monthNumber
        |> String.fromInt
        |> String.padLeft 2 '0'
        |> (\m ->
                m ++ " / " ++ String.fromInt year
           )
        |> (\y ->
                if String.contains "1970" y then
                    "AGES AGO"

                else
                    y
           )



-- GROUPING  ░░  YEAR


groupByYear : CollectionDependencies -> Collection -> Collection
groupByYear deps collection =
    collection.identified
        |> List.foldl groupByYear_ Dict.empty
        |> Dict.values
        |> List.reverse
        |> List.concatMap (Sorting.sort deps.sortBy deps.sortDirection >> List.indexedMap setIndexInGroup)
        |> (\arranged -> { collection | arranged = arranged })


groupByYear_ : IdentifiedTrack -> Dict Int (List IdentifiedTrack) -> Dict Int (List IdentifiedTrack)
groupByYear_ ( i, t ) =
    let
        group =
            { name = Maybe.unwrap "0000 - Unknown" String.fromInt t.tags.year
            , index = 0
            }

        item =
            ( { i | group = Just group }
            , t
            )
    in
    Dict.update
        (Maybe.withDefault 0 t.tags.year)
        (\maybeList ->
            case maybeList of
                Just list ->
                    Just (item :: list)

                Nothing ->
                    Just [ item ]
        )
