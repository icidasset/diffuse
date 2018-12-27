module UI.UserData exposing (HypaethralBundle, exportHypaethral, importHypaethral)

{-| Import user data into or export user data from the UI.Core.Model
-}

import Authentication exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Sources.Encoding as Sources
import UI.Core
import UI.Sources



-----------------------------------------
-- 📭
-----------------------------------------


type alias HypaethralBundle =
    ( HypaethralUserData, Decode.Value )


importHypaethral : Decode.Value -> UI.Core.Model -> UI.Core.Model
importHypaethral value model =
    let
        data =
            Result.withDefault emptyHypaethralUserData (decode value)

        bundle =
            ( data
            , Encode.null
            )
    in
    { model | sources = importSources model.sources bundle }



-- 📭  |  Importing Hypaethral


importSources : UI.Sources.Model -> HypaethralBundle -> UI.Sources.Model
importSources model ( data, _ ) =
    { model | collection = Maybe.withDefault [] data.sources }



-- 📭  |  Decoding


decode : Decode.Value -> Result Decode.Error HypaethralUserData
decode =
    Decode.decodeValue decoder


decoder : Decode.Decoder HypaethralUserData
decoder =
    Decode.map
        HypaethralUserData
        (Decode.maybe <| Decode.field "sources" <| Decode.list Sources.decoder)



-- 📭  |  Fallbacks


emptyHypaethralUserData : HypaethralUserData
emptyHypaethralUserData =
    { sources = Nothing }



-----------------------------------------
-- 📮
-----------------------------------------


exportHypaethral : UI.Core.Model -> Encode.Value
exportHypaethral =
    encode



-- 📮  |  Encoding


encode : UI.Core.Model -> Encode.Value
encode model =
    Encode.object
        [ ( "sources", Encode.list Sources.encode model.sources.collection ) ]
