module Tracks.Selecting exposing (..)

import List.Extra as List


type alias Args =
    { alreadySelected : List Int
    , holdingShiftKey : Bool
    , targetTrackIndex : Int
    }



-- Regular


inTable : Args -> List Int
inTable { alreadySelected, holdingShiftKey, targetTrackIndex } =
    if List.isEmpty alreadySelected then
        [ targetTrackIndex ]
    else if not holdingShiftKey then
        alreadySelected
            |> List.head
            |> Maybe.map (selectSingle targetTrackIndex)
            |> Maybe.withDefault [ targetTrackIndex ]
    else
        alreadySelected
            |> List.head
            |> Maybe.map (selectMultiple targetTrackIndex)
            |> Maybe.withDefault [ targetTrackIndex ]


selectSingle : Int -> Int -> List Int
selectSingle target origin =
    if target == origin then
        []
    else
        [ target ]


selectMultiple : Int -> Int -> List Int
selectMultiple target origin =
    if target == origin then
        []
    else if target > origin then
        List.range origin target
    else
        List.reverse (List.range target origin)



-- Right click


rightClickInTable : Args -> List Int
rightClickInTable { alreadySelected, holdingShiftKey, targetTrackIndex } =
    if List.length alreadySelected < 2 then
        [ targetTrackIndex ]
    else if List.member targetTrackIndex alreadySelected then
        alreadySelected
    else
        [ targetTrackIndex ]
