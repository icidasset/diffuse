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
import Return3 as Return exposing (..)
import Task.Extra exposing (do)



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


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        -- 🐤
        -- Set & store method,
        -- and retrieve data.
        PerformSignIn json ->
            let
                decoder =
                    Decode.map2
                        Tuple.pair
                        (Decode.field "method" <| Decode.map methodFromString Decode.string)
                        (Decode.field "passphrase" <| Decode.maybe Decode.string)
            in
            case Decode.decodeValue decoder json of
                Ok ( maybeMethod, Just passphrase ) ->
                    update
                        (FabricateSecretKey passphrase)
                        { model | method = maybeMethod, performingSignIn = True }

                Ok ( maybeMethod, Nothing ) ->
                    update
                        RetrieveHypaethralData
                        { model | method = maybeMethod, performingSignIn = True }

                _ ->
                    return model

        -- 💀
        -- Unset & remove stored method.
        PerformSignOut ->
            [ Ports.removeCache (Alien.trigger Alien.AuthMethod)
            , Ports.removeCache (Alien.trigger Alien.AuthSecretKey)
            ]
                |> Cmd.batch
                |> Return.commandWithModel { model | method = Nothing }

        -----------------------------------------
        -- # 0
        -----------------------------------------
        FabricateSecretKey passphrase ->
            passphrase
                |> Json.string
                |> Alien.broadcast Alien.FabricateSecretKey
                |> Ports.fabricateSecretKey
                |> Return.commandWithModel model

        SecretKeyFabricated ->
            if model.performingSignIn then
                update RetrieveHypaethralData model

            else
                returnReplyWithModel model FabricatedNewSecretKey

        -----------------------------------------
        -- # 1
        -----------------------------------------
        RetrieveMethod ->
            Alien.AuthMethod
                |> Alien.trigger
                |> Ports.requestCache
                |> Return.commandWithModel model

        MethodRetrieved json ->
            case decodeMethod json of
                -- 🚀
                Just method ->
                    update
                        RetrieveHypaethralData
                        { model | method = Just method }

                -- ✋
                _ ->
                    addReplies
                        (terminate NotAuthenticated)
                        (return model)

        -----------------------------------------
        -- # 2
        -----------------------------------------
        RetrieveHypaethralData ->
            case model.method of
                -- 🚀
                Just Blockstack ->
                    Alien.AuthBlockstack
                        |> Alien.trigger
                        |> Ports.requestBlockstack
                        |> Return.commandWithModel model

                Just (Ipfs { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthIpfs
                        |> Ports.requestIpfs
                        |> Return.commandWithModel model

                Just Local ->
                    Alien.AuthAnonymous
                        |> Alien.trigger
                        |> Ports.requestCache
                        |> Return.commandWithModel model

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "token", Json.string token )
                    , ( "userAddress", Json.string userAddress )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthRemoteStorage
                        |> Ports.requestRemoteStorage
                        |> Return.commandWithModel model

                Just (Textile { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthTextile
                        |> Ports.requestTextile
                        |> Return.commandWithModel model

                -- ✋
                Nothing ->
                    return model

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
            , model.method
                |> Maybe.map (\method -> Authenticated method json)
                |> Maybe.map terminate
                |> Maybe.withDefault []
            )

        -----------------------------------------
        -- Data
        -----------------------------------------
        RetrieveEnclosedData ->
            Alien.AuthEnclosedData
                |> Alien.trigger
                |> Ports.requestCache
                |> Return.commandWithModel model

        EnclosedDataRetrieved json ->
            json
                |> GiveUI Alien.LoadEnclosedUserData
                |> Return.replyWithModel model

        SaveEnclosedData json ->
            json
                |> Alien.broadcast Alien.AuthEnclosedData
                |> Ports.toCache
                |> Return.commandWithModel model

        SaveHypaethralData json ->
            case model.method of
                -- 🚀
                Just Blockstack ->
                    json
                        |> Alien.broadcast Alien.AuthBlockstack
                        |> Ports.toBlockstack
                        |> Return.commandWithModel model

                Just (Ipfs { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    , ( "data", json )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthIpfs
                        |> Ports.toIpfs
                        |> Return.commandWithModel model

                Just Local ->
                    json
                        |> Alien.broadcast Alien.AuthAnonymous
                        |> Ports.toCache
                        |> Return.commandWithModel model

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "data", json )
                    , ( "token", Json.string token )
                    , ( "userAddress", Json.string userAddress )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthRemoteStorage
                        |> Ports.toRemoteStorage
                        |> Return.commandWithModel model

                Just (Textile { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    , ( "data", json )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthTextile
                        |> Ports.toTextile
                        |> Return.commandWithModel model

                -- ✋
                Nothing ->
                    return model



-- TERMINATION


type Termination
    = Authenticated Method Json.Value
    | NotAuthenticated


terminate : Termination -> List Reply
terminate t =
    case t of
        Authenticated method hypData ->
            [ GiveUI Alien.LoadHypaethralUserData hypData
            , GiveUI Alien.AuthMethod (Authentication.encodeMethod method)
            ]

        NotAuthenticated ->
            [ NudgeUI Alien.NotAuthenticated
            , NudgeUI Alien.HideLoadingScreen
            ]
