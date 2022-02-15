module Url.Ext exposing (action, extractQueryParam, queryDictionary)

import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Url exposing (Url)
import Url.Parser as Url
import Url.Parser.Query as Query



-- 🔱


action : Url -> List String
action url =
    url
        |> extractQueryParam "action"
        |> Maybe.map (String.split "/")
        |> Maybe.withDefault []


extractQueryParam : String -> Url -> Maybe String
extractQueryParam key url =
    { url | path = "" }
        |> Url.parse (Url.query (Query.string key))
        |> Maybe.join


queryDictionary : Url -> Dict String String
queryDictionary url =
    url.query
        |> Maybe.map (String.split "&")
        |> Maybe.withDefault []
        |> List.filterMap
            (\s ->
                case String.split "=" s of
                    [ k, v ] ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )
        |> Dict.fromList
