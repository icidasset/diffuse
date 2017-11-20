module Alfred.System exposing (..)

import Response.Ext exposing (do)
import Types exposing (Alfred)


calculateResults : String -> Alfred -> Alfred
calculateResults searchTerm alfred =
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
                    List.filter
                        (String.toLower >> String.contains lowerSearchTerm)
                        alfred.index
            }
        else
            { alfred
                | searchTerm = Nothing
                , results = []
            }


runAction : Alfred -> ( Alfred, Cmd Types.Msg )
runAction alfred =
    ( alfred
    , alfred.results
        |> List.head
        |> alfred.action alfred.searchTerm
    )
