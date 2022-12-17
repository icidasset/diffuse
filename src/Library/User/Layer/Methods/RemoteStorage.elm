module User.Layer.Methods.RemoteStorage exposing (Attributes, oauthAddress, parseUserAddress, userAddressError, webfingerAddress, webfingerDecoder, webfingerError, webfingerRequest)

import Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Url
import UrlBase64



-- 🌳


type alias Attributes =
    { host : String
    , username : String
    }


userAddressError =
    "Please provide a valid RemoteStorage address, the format is **user@server**"


webfingerError =
    "**Failed to connect** to the given RemoteStorage server, maybe a typo?"



-- 🔱


parseUserAddress : String -> Maybe Attributes
parseUserAddress str =
    case String.split "@" str of
        [ u, h ] ->
            Just { host = h, username = u }

        _ ->
            Nothing


oauthAddress : { oauthOrigin : String, origin : String } -> Attributes -> String
oauthAddress { oauthOrigin, origin } { host, username } =
    let
        hostWithoutProtocol =
            host
                |> String.split "://"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault host

        ua =
            (username ++ "@" ++ hostWithoutProtocol)
                |> UrlBase64.encode (Base64.encode >> Ok)
                |> Result.withDefault "BASE64_ENCODING_FAILED"
    in
    String.concat
        [ oauthOrigin
        , "?redirect_uri=" ++ Url.percentEncode (origin ++ "?action=authenticate/remotestorage/" ++ ua)
        , "&client_id=" ++ Url.percentEncode origin
        , "&scope=" ++ Url.percentEncode "diffuse:rw"
        , "&response_type=token"
        ]


webfingerAddress : Url.Protocol -> Attributes -> String
webfingerAddress originProtocol { host, username } =
    let
        fallbackProtocol =
            case originProtocol of
                Url.Http ->
                    "http"

                Url.Https ->
                    "https"

        protocol =
            if String.contains "://" host then
                host
                    |> String.split "://"
                    |> List.head
                    |> Maybe.withDefault fallbackProtocol

            else
                fallbackProtocol

        hostWithoutProtocol =
            host
                |> String.split "://"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault host
    in
    protocol ++ "://" ++ hostWithoutProtocol ++ "/.well-known/webfinger?resource=acct:" ++ Url.percentEncode (username ++ "@" ++ hostWithoutProtocol)


webfingerDecoder : Decoder String
webfingerDecoder =
    Decode.at
        [ "links"
        , "0"
        , "properties"
        , "http://tools.ietf.org/html/rfc6749#section-4.2"
        ]
        Decode.string


webfingerRequest : (Attributes -> Result Http.Error String -> msg) -> Url.Protocol -> Attributes -> Cmd msg
webfingerRequest toMsg originProtocol rs =
    Http.get
        { url = webfingerAddress originProtocol rs
        , expect = Http.expectJson (toMsg rs) webfingerDecoder
        }
