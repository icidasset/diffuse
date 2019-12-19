module Sources.Services.Google.Marker exposing (..)

{-| Custom `Marker` for the Google Drive API.

The Google Drive API currently doesn't make a recursive list,
so we have to manage that ourselves.

This custom marker is a combination of:

  - The standard `nextPageToken` param
  - Our custom logic to handle recursive listings

Example: InProgress "dir=example ¶ param=defaultMarker"

-}

import Sources.Processing exposing (Marker(..))


type MarkerItem
    = Directory String
    | Param { directory : String, token : String }


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


itemDirectory : MarkerItem -> String
itemDirectory item =
    case item of
        Directory dir ->
            dir

        Param { directory } ->
            directory


itemToString : MarkerItem -> String
itemToString item =
    case item of
        Directory d ->
            "dir" ++ prefixer ++ d

        Param { directory, token } ->
            "par" ++ prefixer ++ directory ++ paramSeparator ++ token


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
                            [ dir, tok ] ->
                                Just (Param { directory = dir, token = tok })

                            _ ->
                                Nothing
                   )

        _ ->
            Nothing
