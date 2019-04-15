module Authentication.RemoteStorage exposing (RemoteStorage, oauthAddress, parseUserAddress)

import Base64
import Url



-- 🌳


type alias RemoteStorage =
    { host : String
    , username : String
    }



-- 🔱


parseUserAddress : String -> Maybe RemoteStorage
parseUserAddress str =
    case String.split "@" str of
        [ u, h ] ->
            Just { host = h, username = u }

        _ ->
            Nothing


oauthAddress : { origin : String } -> RemoteStorage -> String
oauthAddress { origin } { host, username } =
    let
        ua =
            (username ++ "@" ++ host)
                |> Base64.encode
                |> Url.percentEncode
    in
    String.concat
        [ "https://" ++ host ++ "/rs/oauth/" ++ username
        , "?redirect_uri=" ++ Url.percentEncode (origin ++ "/authenticate/remotestorage/" ++ ua)
        , "&client_id=" ++ Url.percentEncode origin
        , "&scope=" ++ Url.percentEncode "diffuse-v2:rw"
        , "&response_type=token"
        ]


webfingerAddress : RemoteStorage -> String
webfingerAddress { host, username } =
    "https://" ++ host ++ "/.well-known/webfinger?resource=acct:" ++ username
