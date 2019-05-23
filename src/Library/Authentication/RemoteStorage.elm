module Authentication.RemoteStorage exposing (RemoteStorage, oauthAddress, parseUserAddress, userAddressError, webfingerAddress, webfingerDecoder, webfingerError, webfingerRequest)

import Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Url



-- 🌳


type alias RemoteStorage =
    { host : String
    , username : String
    }


userAddressError =
    "Please provide a valid RemoteStorage address, the format is **user@server**"


webfingerError =
    "**Failed to connect** to the given RemoteStorage server, maybe a typo?"



-- 🔱


parseUserAddress : String -> Maybe RemoteStorage
parseUserAddress str =
    case String.split "@" str of
        [ u, h ] ->
            Just { host = h, username = u }

        _ ->
            Nothing


oauthAddress : { oauthOrigin : String, origin : String } -> RemoteStorage -> String
oauthAddress { oauthOrigin, origin } { host, username } =
    let
        ua =
            (username ++ "@" ++ host)
                |> Base64.encode
                |> Url.percentEncode
    in
    String.concat
        [ oauthOrigin
        , "?redirect_uri=" ++ Url.percentEncode (origin ++ "/authenticate/remotestorage/" ++ ua)
        , "&client_id=" ++ Url.percentEncode origin
        , "&scope=" ++ Url.percentEncode "diffuse-v2:rw"
        , "&response_type=token"
        ]


webfingerAddress : RemoteStorage -> String
webfingerAddress { host, username } =
    "https://" ++ host ++ "/.well-known/webfinger?resource=acct:" ++ username


webfingerDecoder : Decoder String
webfingerDecoder =
    Decode.at
        [ "links"
        , "0"
        , "properties"
        , "http://tools.ietf.org/html/rfc6749#section-4.2"
        ]
        Decode.string


webfingerRequest : (RemoteStorage -> Result Http.Error String -> msg) -> RemoteStorage -> Cmd msg
webfingerRequest toMsg rs =
    Http.get
        { url = webfingerAddress rs
        , expect = Http.expectJson (toMsg rs) webfingerDecoder
        }
