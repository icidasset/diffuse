module User.Layer.Methods.Dropbox exposing (..)

import Common
import Http
import Json.Decode as Json
import Url exposing (Url)



-- 🌳


type TokenFlow
    = Code
    | Refresh


type alias Tokens =
    { accessToken : String
    , expiresIn : Int -- Time in seconds the access token expires in
    , refreshToken : Maybe String
    }



-- 🏔


clientId : String
clientId =
    "te0c9pbeii8f8bw"


clientSecret : String
clientSecret =
    "kxmlfdsw8k9e0ot"


redirectUri : Url -> String
redirectUri url =
    Common.urlOrigin url ++ "?action=authenticate/dropbox"



-- ENCODING


tokensDecoder : Json.Decoder Tokens
tokensDecoder =
    Json.map3
        (\a e r ->
            { accessToken = a
            , expiresIn = e
            , refreshToken = r
            }
        )
        (Json.field "access_token" Json.string)
        (Json.field "expires_in" Json.int)
        (Json.string
            |> Json.field "refresh_token"
            |> Json.maybe
        )



-- 🛠


exchangeAuthCode : (Result Http.Error Tokens -> msg) -> Url -> String -> Cmd msg
exchangeAuthCode msg url code =
    [ ( "client_id", clientId )
    , ( "client_secret", clientSecret )
    , ( "code", code )
    , ( "grant_type", "authorization_code" )
    , ( "redirect_uri", redirectUri url )
    ]
        |> Common.queryString
        |> String.append "https://api.dropboxapi.com/oauth2/token"
        |> (\u ->
                { url = u
                , body = Http.emptyBody
                , expect = Http.expectJson msg tokensDecoder
                }
           )
        |> Http.post
