module Sources.Services.Common exposing (cleanPath, noPrep)

import Sources exposing (..)
import Sources.Processing exposing (..)
import String.Ext as String



-- PATHS


{-| Clean a path.

    >>> cleanPath "   "
    ""

    >>> cleanPath "/example"
    "example/"

    >>> cleanPath "example"
    "example/"

    >>> cleanPath "example/"
    "example/"

-}
cleanPath : String -> String
cleanPath dirtyPath =
    dirtyPath
        |> String.trim
        |> String.chopStart "/"
        |> String.chopEnd "/"
        |> (\p ->
                if String.isEmpty p then
                    p

                else
                    p ++ "/"
           )



-- PARSING


noPrep : String -> SourceData -> Marker -> PrepationAnswer Marker
noPrep _ srcData _ =
    { sourceData = srcData, marker = TheEnd }
