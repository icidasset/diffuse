module UI.Sources.Query exposing (..)

import Dict
import Json.Decode as Decode
import Sources exposing (Source)
import Url exposing (Url)
import Url.Parser as Url
import Url.Parser.Query as Query


requestedAddition : Url -> Bool
requestedAddition url =
    case Url.parse (urlParser identity) url of
        Nothing ->
            False

        Just [] ->
            False

        Just (_ :: _) ->
            True


sourcesFromUrl : Url -> List Source
sourcesFromUrl url =
    url
        |> Url.parse (urlParser <| List.filterMap fromUrl)
        |> Maybe.withDefault []



-- 🔬


fromUrl : String -> Maybe Source
fromUrl json =
    json
        |> Decode.decodeString sourceParser
        |> Result.toMaybe


urlParser individualParser =
    individualParser
        |> Query.custom "source"
        |> Url.query


sourceParser : Decode.Decoder Source
sourceParser =
    Decode.andThen
        (\{ service, data } ->
            if Dict.member "name" data then
                Decode.succeed
                    { id = "FILL_IN_LATER"
                    , data = data
                    , directoryPlaylists = True
                    , enabled = True
                    , service = service
                    }

            else
                Decode.fail "Missing `name` in `data` dictionary"
        )
    <|
        Decode.map2
            (\s d -> { service = s, data = d })
            (Decode.field "kind" Sources.serviceDecoder)
            (Decode.field "data" <| Decode.dict Decode.string)
