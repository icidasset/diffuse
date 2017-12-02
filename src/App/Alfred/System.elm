module Alfred.System exposing (..)

import List.Extra as List
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
                    alfred.index
                        |> List.filter (String.toLower >> String.contains lowerSearchTerm)
                        |> List.sort
            }
        else
            { alfred
                | searchTerm = Nothing
                , results = []
            }


runAction : Int -> Alfred -> ( Alfred, Cmd Types.Msg )
runAction index alfred =
    ( alfred
    , alfred.results
        |> List.getAt index
        |> alfred.action alfred.searchTerm
    )
