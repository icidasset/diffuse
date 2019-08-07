module Sources.Services.Azure.FileMarker exposing (MarkerItem(..), concat, itemToString, paramSeparator, prefixer, removeOne, separator, stringToItem, takeOne)

{-| Custom `Marker` for the Azure File API.

The Azure File API currently doesn't make a recursive list,
so we have to manage that ourselves.

This custom marker is a combination of:

  - The default `marker` param, see URI parameters at <https://docs.microsoft.com/en-us/rest/api/storageservices/list-directories-and-files>
  - Our custom logic to handle recursive listings

Example: InProgress "dir=example ¶ param=defaultMarker"

-}

import Sources.Processing exposing (Marker(..))


type MarkerItem
    = Directory String
    | Param { directory : String, marker : String }


separator : String
separator =
    " ɑ "


prefixer : String
prefixer =
    " β "


paramSeparator : String
paramSeparator =
    " ɣ "



-- IN


concat : List MarkerItem -> Marker -> Marker
concat list marker =
    let
        listStringified =
            List.map itemToString list

        result =
            case marker of
                InProgress m ->
                    [ listStringified, String.split separator m ]
                        |> List.concat
                        |> String.join separator

                _ ->
                    String.join separator listStringified
    in
    case result of
        "" ->
            TheEnd

        r ->
            InProgress r



-- OUT


{-| Take the first item and return it.
-}
takeOne : Marker -> Maybe MarkerItem
takeOne marker =
    case marker of
        InProgress m ->
            m
                |> String.split separator
                |> List.head
                |> Maybe.andThen stringToItem

        _ ->
            Nothing


{-| Remove the first item if there is one.
-}
removeOne : Marker -> Marker
removeOne marker =
    case marker of
        InProgress m ->
            let
                tmp =
                    m
                        |> String.split separator
                        |> List.drop 1
                        |> String.join separator
            in
            case tmp of
                "" ->
                    TheEnd

                x ->
                    InProgress x

        _ ->
            TheEnd



-- CONVERSIONS


itemToString : MarkerItem -> String
itemToString item =
    case item of
        Directory d ->
            "dir" ++ prefixer ++ d

        Param { directory, marker } ->
            "par" ++ prefixer ++ directory ++ paramSeparator ++ marker


stringToItem : String -> Maybe MarkerItem
stringToItem string =
    let
        exploded =
            String.split prefixer string
    in
    case List.head exploded of
        Just "dir" ->
            exploded
                |> List.drop 1
                |> String.join prefixer
                |> Directory
                |> Just

        Just "par" ->
            exploded
                |> List.drop 1
                |> String.join prefixer
                |> String.split paramSeparator
                |> (\x ->
                        case x of
                            [ dir, mar ] ->
                                Just (Param { directory = dir, marker = mar })

                            _ ->
                                Nothing
                   )

        _ ->
            Nothing
