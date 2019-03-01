module Brain.Authentication exposing (Model, Msg(..), initialCommand, initialModel, update)

{-| Authentication.

Involves:

    1. Enclosed UserData (enable shuffle, EQ settings, ... ~> device)
    2. Hypaethral UserData (sources, tracks, favourites, ... ~> account)

Methods:

    - Local
    - RemoteStorage
    - IPNS

Steps:

    1. Get active method (if none, we're signed out)
    2. Get hypaethral data

-}

import Alien
import Array
import Authentication exposing (..)
import Base64
import Brain.Ports as Ports
import Brain.Reply exposing (Reply(..))
import Bytes
import Bytes.Decode
import Bytes.Encode
import Conditional exposing (..)
import Crypto.AES as Aes
import Hex
import Json.Decode as J
import Json.Encode
import List.Extra as List
import Replying exposing (R3D3, do)
import String.Ext as String
import String.UTF8



-- 🌳


type alias Model =
    { method : Maybe Method
    }


initialModel : Model
initialModel =
    { method = Nothing
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.batch
        [ do RetrieveMethod
        , do RetrieveEnclosedData
        ]



-- 📣


type Msg
    = PerformSignIn J.Value
    | PerformSignOut
      -- 1. Method
    | RetrieveMethod
    | MethodRetrieved J.Value
      -- 2. Data
    | RetrieveHypaethralData
    | HypaethralDataRetrieved J.Value
      -- x. Data
    | RetrieveEnclosedData
    | EnclosedDataRetrieved J.Value
    | SaveEnclosedData J.Value
    | SaveHypaethralData J.Value


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        -- 🐤
        -- Set & store method,
        -- and retrieve data.
        PerformSignIn json ->
            case decodeMethod json of
                Just method ->
                    ( { model | method = Just method }
                    , Cmd.batch
                        [ do RetrieveHypaethralData
                        , json
                            |> Alien.broadcast Alien.AuthMethod
                            |> Ports.toCache
                        ]
                    , Nothing
                    )

                Nothing ->
                    ( model
                    , noCmd
                    , Nothing
                    )

        -- 💀
        -- Unset & remove stored method.
        PerformSignOut ->
            ( { model | method = Nothing }
            , Alien.AuthMethod
                |> Alien.trigger
                |> Ports.removeCache
            , Nothing
            )

        -----------------------------------------
        -- # 1
        -----------------------------------------
        RetrieveMethod ->
            ( model
            , Alien.AuthMethod
                |> Alien.trigger
                |> Ports.requestCache
            , Nothing
            )

        MethodRetrieved json ->
            case decodeMethod json of
                -- 🚀
                Just method ->
                    ( { model | method = Just method }
                    , do RetrieveHypaethralData
                    , Nothing
                    )

                -- ✋
                _ ->
                    ( model
                    , noCmd
                    , terminate NotAuthenticated
                    )

        -----------------------------------------
        -- # 2
        -----------------------------------------
        RetrieveHypaethralData ->
            ( model
            , case model.method of
                -- 🚀
                Just (Ipfs _) ->
                    Ports.requestIpfs (Alien.trigger Alien.AuthIpfs)

                Just Local ->
                    Ports.requestCache (Alien.trigger Alien.AuthAnonymous)

                -- ✋
                Nothing ->
                    noCmd
            , Nothing
            )

        HypaethralDataRetrieved json ->
            ( model
            , noCmd
            , Maybe.andThen
                (\method ->
                    case method of
                        Ipfs { encryptionKey } ->
                            let
                                decoder =
                                    decryptionDecoder encryptionKey
                            in
                            json
                                |> J.decodeValue (J.andThen decoder J.string)
                                |> Result.andThen (J.decodeString J.value)
                                |> Result.withDefault Json.Encode.null
                                |> Authenticated method
                                |> terminate

                        Local ->
                            terminate (Authenticated method json)
                )
                model.method
            )

        -----------------------------------------
        -- Data
        -----------------------------------------
        RetrieveEnclosedData ->
            ( model
            , Ports.requestCache (Alien.trigger Alien.AuthEnclosedData)
            , Nothing
            )

        EnclosedDataRetrieved json ->
            ( model
            , noCmd
            , Just [ GiveUI Alien.LoadEnclosedUserData json ]
            )

        SaveEnclosedData json ->
            ( model
            , json
                |> Alien.broadcast Alien.AuthEnclosedData
                |> Ports.toCache
            , Nothing
            )

        SaveHypaethralData json ->
            ( model
            , case model.method of
                -- 🚀
                Just (Ipfs { encryptionKey }) ->
                    -- TODO: Show error if encryption failed
                    json
                        |> Json.Encode.encode 0
                        |> encrypt encryptionKey
                        |> Result.map Json.Encode.string
                        |> Result.map (Alien.broadcast Alien.AuthIpfs)
                        |> Result.map Ports.toIpfs
                        |> Result.withDefault Cmd.none

                Just Local ->
                    Ports.toCache (Alien.broadcast Alien.AuthAnonymous json)

                -- ✋
                Nothing ->
                    noCmd
            , Nothing
            )


noCmd : Cmd msg
noCmd =
    Cmd.none



-- ENCRYPTION


decrypt : String -> String -> Result String String
decrypt key str =
    -- TODO: Ensure that key is hexadecimal and is of the size: 32, 48 or 64 bytes
    key
        |> Aes.expandKeyString
        |> Result.andThen (decrypt_ str)


decrypt_ : String -> Aes.Keys -> Result String String
decrypt_ str keys =
    str
        |> String.foldr
            (\c ( secondChar, acc ) ->
                case secondChar of
                    Just s ->
                        [ c, s ]
                            |> String.fromList
                            |> Hex.fromString
                            |> Result.withDefault 0
                            |> (\r -> r :: acc)
                            |> Tuple.pair Nothing

                    Nothing ->
                        ( Just c, acc )
            )
            ( Nothing, [] )
        |> Tuple.second
        |> List.groupsOf 16
        |> List.concatMap (Array.fromList >> Aes.decrypt keys >> Array.toList)
        |> List.dropWhileRight ((==) 0)
        |> String.UTF8.toString


decryptionDecoder : String -> String -> J.Decoder String
decryptionDecoder encryptionKey encryptedString =
    case decrypt encryptionKey encryptedString of
        Ok decryptedString ->
            J.succeed decryptedString

        Err err ->
            J.fail err


encrypt : String -> String -> Result String String
encrypt key str =
    -- TODO: Ensure that key is hexadecimal and is of the size: 32, 48 or 64 bytes
    key
        |> Aes.expandKeyString
        |> Result.andThen (encrypt_ str)


encrypt_ : String -> Aes.Keys -> Result String String
encrypt_ str keys =
    str
        |> String.UTF8.toBytes
        |> List.greedyGroupsOf 16
        |> List.foldl
            (\group acc ->
                let
                    groupLength =
                        List.length group

                    fixedGroup =
                        if groupLength < 16 then
                            group ++ List.repeat (16 - groupLength) 0

                        else
                            group
                in
                fixedGroup
                    |> Array.fromList
                    |> Aes.encrypt keys
                    |> Array.toList
                    |> List.map (Hex.toString >> String.padLeft 2 '0')
                    |> String.concat
                    |> (\encodedGroup ->
                            Result.map (\a -> String.append a encodedGroup) acc
                       )
            )
            (Result.Ok "")



-- JSON


decodeMethod : J.Value -> Maybe Method
decodeMethod json =
    json
        |> J.decodeValue J.string
        |> Result.toMaybe
        |> Maybe.andThen Authentication.methodFromString



-- TERMINATION


type Termination
    = Authenticated Method J.Value
    | NotAuthenticated


terminate : Termination -> Maybe (List Reply)
terminate t =
    case t of
        Authenticated method hypData ->
            Just
                [ GiveUI Alien.LoadHypaethralUserData hypData
                , GiveUI Alien.AuthMethod (Authentication.encodeMethod method)
                ]

        NotAuthenticated ->
            Just
                [ NudgeUI Alien.HideLoadingScreen ]
