module Common exposing (urlOrigin)

import Url exposing (Protocol(..), Url)



-- 🔱


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
