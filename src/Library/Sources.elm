module Sources exposing (..)

import Conditional exposing (..)
import Dict exposing (Dict)
import Json.Decode
import Time



-- 🌳


type alias Source =
    { id : String
    , data : SourceData
    , directoryPlaylists : Bool
    , enabled : Bool
    , service : Service
    }



-- PIECES


type alias Property =
    { key : String
    , label : String
    , placeholder : String
    , password : Bool
    }


type alias SourceData =
    Dict String String



-- SERVICES


type Service
    = AmazonS3
    | AzureBlob
    | AzureFile
    | Btfs
    | Dropbox
    | Google
    | Ipfs
    | WebDav


serviceDictionary : Dict String Service
serviceDictionary =
    Dict.fromList
        [ ( "amazons3", AmazonS3 )
        , ( "amazon_s3", AmazonS3 )
        , ( "azureblob", AzureBlob )
        , ( "azure_blob", AzureBlob )
        , ( "azurefile", AzureFile )
        , ( "azure_file", AzureFile )
        , ( "btfs", Btfs )
        , ( "dropbox", Dropbox )
        , ( "google", Google )
        , ( "ipfs", Ipfs )
        , ( "webdav", WebDav )
        , ( "web_dav", WebDav )
        ]


serviceDecoder : Json.Decode.Decoder Service
serviceDecoder =
    Json.Decode.andThen
        (\string ->
            serviceDictionary
                |> Dict.get string
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail "Invalid source kind")
        )
        Json.Decode.string



--- 🔱


enabledSourceIds : List Source -> List String
enabledSourceIds =
    List.filterMap (\s -> ifThenElse s.enabled (Just s.id) Nothing)


setProperId : Int -> Time.Posix -> Source -> Source
setProperId n time source =
    { source | id = String.fromInt (Time.posixToMillis time) ++ String.fromInt n }
