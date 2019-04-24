module Sources.Services.Ipfs.Marker exposing (concat, removeOne, separator, takeOne)

{-| Marker stuff for IPFS.

The IPFS API currently doesn't have a way to return a tree.
So we have build one ourselves.

How it works:

1.  We list the objects in a given directory
2.  We make a list of the sub directories
3.  The marker becomes either:
      - InProgress `hashOfSubDirA/hashOfSubDirB/hashOfSubDirC`
      - TheEnd
4.  If the marker was of the type `InProgress`,
    the next request will make a list of the objects in `hashOfSubDirA`.
    And so on ...

-}

import Sources.Processing exposing (Marker(..))


separator : String
separator =
    " ɑ "



-- In


concat : List String -> Marker -> Marker
concat list marker =
    let
        result =
            case marker of
                InProgress m ->
                    [ list, String.split separator m ]
                        |> List.concat
                        |> String.join separator

                _ ->
                    String.join separator list
    in
    case result of
        "" ->
            TheEnd

        r ->
            InProgress r



-- Out


{-| Take the first item and return it.
-}
takeOne : Marker -> Maybe String
takeOne marker =
    case marker of
        InProgress m ->
            m
                |> String.split separator
                |> List.head

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
