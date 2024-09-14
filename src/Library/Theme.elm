module Theme exposing (..)

import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Material.Icons.Types exposing (Icon)


type alias Id =
    { id : String }


type alias Theme msg model =
    { id : String
    , title : String
    , icon : Icon msg
    , view : model -> Html msg
    }


idDecoder : Decode.Decoder Id
idDecoder =
    Decode.map (\s -> { id = s }) Decode.string


encodeId : Id -> Encode.Value
encodeId { id } =
    Encode.string id
