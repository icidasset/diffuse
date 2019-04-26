module Common exposing (Switch(..), queryString, urlOrigin)

import Tuple.Ext as Tuple
import Url exposing (Protocol(..), Url)
import Url.Builder as Url



-- 🌳


type Switch
    = On
    | Off



-- 🔱


queryString : List ( String, String ) -> String
queryString =
    List.map (Tuple.uncurry Url.string) >> Url.toQuery


urlOrigin : Url -> String
urlOrigin { host, port_, protocol } =
    let
        scheme =
            case protocol of
                Http ->
                    "http://"

                Https ->
                    "https://"

        thePort =
            port_
                |> Maybe.map (String.fromInt >> (++) ":")
                |> Maybe.withDefault ""
    in
    scheme ++ host ++ thePort
