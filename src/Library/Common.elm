module Common exposing (Switch(..), urlOrigin)

import Url exposing (Protocol(..), Url)



-- 🌳


type Switch
    = On
    | Off



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
