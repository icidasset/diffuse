module User.Layer.Methods.Dropbox exposing (..)

import Common
import Http
import Http.Ext as Http
import Http.Extras as Http
import Json.Decode as Json
import Task exposing (Task)
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


refreshAccessToken : String -> Task String Tokens
refreshAccessToken refreshToken =
    [ ( "client_id", clientId )
    , ( "client_secret", clientSecret )
    , ( "refresh_token", refreshToken )
    , ( "grant_type", "refresh_token" )
    ]
        |> Common.queryString
        |> String.append "https://api.dropboxapi.com/oauth2/token"
        |> (\u ->
                { method = "POST"
                , headers = []
                , url = u
                , body = Http.emptyBody
                , resolver =
                    Http.stringResolver
                        (\resp ->
                            resp
                                |> Http.responseToString
                                |> Result.mapError Http.errorToString
                                |> Result.andThen
                                    (Json.decodeString tokensDecoder
                                        >> Result.mapError Json.errorToString
                                    )
                        )
                , timeout = Nothing
                }
           )
        |> Http.task
