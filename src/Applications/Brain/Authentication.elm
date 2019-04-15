module Brain.Authentication exposing (Model, Msg(..), initialCommand, initialModel, update)

{-| Authentication.

Involves:

    1. Enclosed UserData (enable shuffle, EQ settings, ... ~> device)
    2. Hypaethral UserData (sources, tracks, favourites, ... ~> account)

Methods:

    - Local
    - IPFS

Steps:

    1. Get active method (if none, we're signed out)
    2. Get hypaethral data

-}

import Alien
import Authentication exposing (..)
import Brain.Ports as Ports
import Brain.Reply exposing (Reply(..))
import Conditional exposing (..)
import Json.Decode as Decode
import Json.Encode as Json
import Replying exposing (R3D3, do)



-- 🌳


type alias Model =
    { method : Maybe Method
    , performingSignIn : Bool
    }


initialModel : Model
initialModel =
    { method = Nothing
    , performingSignIn = False
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.batch
        [ do RetrieveMethod
        , do RetrieveEnclosedData
        ]



-- 📣


type Msg
    = PerformSignIn Json.Value
    | PerformSignOut
      -- 0. Secret Key
    | FabricateSecretKey String
    | SecretKeyFabricated
      -- 1. Method
    | RetrieveMethod
    | MethodRetrieved Json.Value
      -- 2. Data
    | RetrieveHypaethralData
    | HypaethralDataRetrieved Json.Value
      -- x. Data
    | RetrieveEnclosedData
    | EnclosedDataRetrieved Json.Value
    | SaveEnclosedData Json.Value
    | SaveHypaethralData Json.Value


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        -- 🐤
        -- Set & store method,
        -- and retrieve data.
        PerformSignIn json ->
            let
                decoder =
                    Decode.map2
                        (\a b -> { maybeMethod = a, maybePassphrase = b })
                        (Decode.field "method" <| Decode.map methodFromString Decode.string)
                        (Decode.field "passphrase" <| Decode.maybe Decode.string)
            in
            case Decode.decodeValue decoder json of
                Ok { maybeMethod, maybePassphrase } ->
                    case maybePassphrase of
                        Just passphrase ->
                            ( { model | method = maybeMethod, performingSignIn = True }
                            , do (FabricateSecretKey passphrase)
                            , Nothing
                            )

                        Nothing ->
                            ( { model | method = maybeMethod, performingSignIn = True }
                            , do RetrieveHypaethralData
                            , Nothing
                            )

                _ ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        -- 💀
        -- Unset & remove stored method.
        PerformSignOut ->
            ( { model | method = Nothing }
            , Cmd.batch
                [ Ports.removeCache (Alien.trigger Alien.AuthMethod)
                , Ports.removeCache (Alien.trigger Alien.AuthSecretKey)
                ]
            , Nothing
            )

        -----------------------------------------
        -- # 0
        -----------------------------------------
        FabricateSecretKey passphrase ->
            ( model
            , passphrase
                |> Json.string
                |> Alien.broadcast Alien.FabricateSecretKey
                |> Ports.fabricateSecretKey
            , Nothing
            )

        SecretKeyFabricated ->
            if model.performingSignIn then
                ( model, do RetrieveHypaethralData, Nothing )

            else
                ( model, Cmd.none, Just [ FabricatedNewSecretKey ] )

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
                    , Cmd.none
                    , terminate NotAuthenticated
                    )

        -----------------------------------------
        -- # 2
        -----------------------------------------
        RetrieveHypaethralData ->
            ( model
            , case model.method of
                -- 🚀
                Just Ipfs ->
                    Ports.requestIpfs (Alien.trigger Alien.AuthIpfs)

                Just Local ->
                    Ports.requestCache (Alien.trigger Alien.AuthAnonymous)

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "token", Json.string token )
                    , ( "userAddress", Json.string userAddress )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthRemoteStorage
                        |> Ports.requestRemoteStorage

                -- ✋
                Nothing ->
                    Cmd.none
            , Nothing
            )

        HypaethralDataRetrieved json ->
            ( { model | performingSignIn = False }
              --
            , case ( model.performingSignIn, model.method ) of
                ( True, Just method ) ->
                    method
                        |> encodeMethod
                        |> Alien.broadcast Alien.AuthMethod
                        |> Ports.toCache

                _ ->
                    Cmd.none
              --
            , Maybe.andThen
                (\method -> terminate <| Authenticated method json)
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
            , Cmd.none
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
                Just Ipfs ->
                    Ports.toIpfs (Alien.broadcast Alien.AuthIpfs json)

                Just Local ->
                    Ports.toCache (Alien.broadcast Alien.AuthAnonymous json)

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "data", json )
                    , ( "token", Json.string token )
                    , ( "userAddress", Json.string userAddress )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthRemoteStorage
                        |> Ports.toRemoteStorage

                -- ✋
                Nothing ->
                    Cmd.none
            , Nothing
            )



-- TERMINATION


type Termination
    = Authenticated Method Json.Value
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
                [ NudgeUI Alien.NotAuthenticated
                , NudgeUI Alien.HideLoadingScreen
                ]
