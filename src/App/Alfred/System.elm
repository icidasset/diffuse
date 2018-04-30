module Alfred.System exposing (..)

import List.Extra as List
import Alfred.Types
import Types as TopLevel


type alias Alfred =
    Alfred.Types.Alfred TopLevel.Msg


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
                , results = alfred.index
            }


runAction : Int -> Alfred -> ( Alfred, Cmd TopLevel.Msg )
runAction index alfred =
    ( alfred
    , alfred.results
        |> List.getAt index
        |> alfred.action alfred.searchTerm
    )
