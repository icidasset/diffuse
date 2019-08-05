module Brain.User.Layer exposing (Model, Msg(..), initialCommand, initialModel, update)

import Alien
import Brain.Ports as Ports
import Brain.Reply exposing (Reply(..))
import Conditional exposing (..)
import Json.Decode as Decode
import Json.Encode as Json
import List.Zipper as Zipper exposing (Zipper)
import Return3 as Return exposing (..)
import Task.Extra exposing (do)
import Url exposing (Url)
import Url.Ext as Url
import User.Layer exposing (..)



-- 🌳


type alias Model =
    { method : Maybe Method
    , hypaethralRetrieval : Maybe (Zipper ( HypaethralBit, Json.Value ))
    , legacyMode : Bool
    , performingSignIn : Bool
    }


initialModel : Model
initialModel =
    { method = Nothing
    , hypaethralRetrieval = Nothing
    , legacyMode = False
    , performingSignIn = False
    }


initialCommand : Url -> Cmd Msg
initialCommand initialUrl =
    case Url.action initialUrl of
        [ "authenticate", "blockstack" ] ->
            case Url.extractQueryParam "authResponse" initialUrl of
                Just authResponse ->
                    Cmd.batch
                        [ do RetrieveEnclosedData
                        , Ports.handlePendingBlockstackSignIn authResponse
                        ]

                Nothing ->
                    Cmd.batch
                        [ do RetrieveMethod
                        , do RetrieveEnclosedData
                        ]

        _ ->
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
    | RetrieveHypaethralData HypaethralBit
    | HypaethralDataRetrieved Json.Value
      -- 2. Data (Legacy)
    | RetrieveLegacyHypaethralData
      -- x. Data
    | RetrieveEnclosedData
    | EnclosedDataRetrieved Json.Value
    | SaveEnclosedData Json.Value
    | SaveHypaethralData HypaethralBit Json.Value


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
                    retrieveAllHypaethralData
                        { model | method = maybeMethod, performingSignIn = True }

                _ ->
                    return model

        -- 💀
        -- Unset & remove stored method.
        PerformSignOut ->
            [ Ports.removeCache (Alien.trigger Alien.AuthMethod)
            , Ports.removeCache (Alien.trigger Alien.AuthSecretKey)

            --
            , case model.method of
                Just Blockstack ->
                    Ports.deconstructBlockstack ()

                Just (Dropbox _) ->
                    Cmd.none

                Just (Ipfs _) ->
                    Cmd.none

                Just Local ->
                    Cmd.none

                Just (RemoteStorage _) ->
                    Ports.deconstructRemoteStorage ()

                Just (Textile _) ->
                    Cmd.none

                Nothing ->
                    Cmd.none
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
                retrieveAllHypaethralData model

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
                    retrieveAllHypaethralData
                        { model | method = Just method }

                -- ✋
                _ ->
                    addReplies
                        (terminate NotAuthenticated)
                        (return model)

        -----------------------------------------
        -- # 2
        -----------------------------------------
        RetrieveHypaethralData bit ->
            let
                file =
                    Json.string (hypaethralBitFileName bit)
            in
            case model.method of
                -- 🚀
                Just Blockstack ->
                    [ ( "file", file ) ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthBlockstack
                        |> Ports.requestBlockstack
                        |> Return.commandWithModel model

                Just (Dropbox { token }) ->
                    [ ( "file", file )
                    , ( "token", Json.string token )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthDropbox
                        |> Ports.requestDropbox
                        |> Return.commandWithModel model

                Just (Ipfs { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    , ( "file", file )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthIpfs
                        |> Ports.requestIpfs
                        |> Return.commandWithModel model

                Just Local ->
                    [ ( "file", file ) ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthAnonymous
                        |> Ports.requestCache
                        |> Return.commandWithModel model

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "file", file )
                    , ( "token", Json.string token )
                    , ( "userAddress", Json.string userAddress )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthRemoteStorage
                        |> Ports.requestRemoteStorage
                        |> Return.commandWithModel model

                Just (Textile { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    , ( "file", file )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthTextile
                        |> Ports.requestTextile
                        |> Return.commandWithModel model

                -- ✋
                Nothing ->
                    return model

        HypaethralDataRetrieved json ->
            if model.legacyMode then
                --------------
                -- Legacy Data
                --------------
                case Decode.decodeValue hypaethralDataDecoder json of
                    Ok hypData ->
                        returnRepliesWithModel
                            { model | legacyMode = False }
                            [ NudgeUI Alien.ImportLegacyData
                            , GiveUI Alien.LoadHypaethralUserData json
                            ]

                    Err _ ->
                        return model

            else
                ---------------
                -- Default Flow
                ---------------
                let
                    retrieval =
                        Maybe.map
                            (Zipper.mapCurrent <| Tuple.mapSecond <| always json)
                            model.hypaethralRetrieval
                in
                case Maybe.andThen Zipper.next retrieval of
                    Just nextRetrieval ->
                        update
                            (RetrieveHypaethralData <| Tuple.first <| Zipper.current nextRetrieval)
                            { model | hypaethralRetrieval = Just nextRetrieval }

                    Nothing ->
                        -- 🚀
                        ( { model
                            | hypaethralRetrieval = Nothing
                            , performingSignIn = False
                          }
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
                        , let
                            allJson =
                                retrieval
                                    |> Maybe.map Zipper.toList
                                    |> Maybe.withDefault []
                                    |> putHypaethralJsonBitsTogether
                          in
                          model.method
                            |> Maybe.map (\method -> Authenticated method allJson)
                            |> Maybe.map terminate
                            |> Maybe.withDefault []
                        )

        -----------------------------------------
        -- # 2 (Legacy)
        -----------------------------------------
        RetrieveLegacyHypaethralData ->
            let
                file =
                    Json.string "diffuse.json"
            in
            case model.method of
                -- 🚀
                Just Blockstack ->
                    [ ( "file", file ) ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthBlockstack
                        |> Ports.requestBlockstack
                        |> Return.commandWithModel { model | legacyMode = True }

                Just Local ->
                    Alien.AuthAnonymous
                        |> Alien.trigger
                        |> Ports.requestLegacyLocalData
                        |> Return.commandWithModel { model | legacyMode = True }

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "file", file )
                    , ( "token", Json.string token )
                    , ( "userAddress", Json.string userAddress )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthRemoteStorage
                        |> Ports.requestRemoteStorage
                        |> Return.commandWithModel { model | legacyMode = True }

                -- ✋
                _ ->
                    return model

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

        SaveHypaethralData bit json ->
            let
                file =
                    Json.string (hypaethralBitFileName bit)
            in
            case model.method of
                -- 🚀
                Just Blockstack ->
                    [ ( "data", json )
                    , ( "file", file )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthBlockstack
                        |> Ports.toBlockstack
                        |> Return.commandWithModel model

                Just (Dropbox { token }) ->
                    [ ( "data", json )
                    , ( "file", file )
                    , ( "token", Json.string token )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthDropbox
                        |> Ports.toDropbox
                        |> Return.commandWithModel model

                Just (Ipfs { apiOrigin }) ->
                    [ ( "apiOrigin", Json.string apiOrigin )
                    , ( "data", json )
                    , ( "file", file )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthIpfs
                        |> Ports.toIpfs
                        |> Return.commandWithModel model

                Just Local ->
                    [ ( "data", json )
                    , ( "file", file )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthAnonymous
                        |> Ports.toCache
                        |> Return.commandWithModel model

                Just (RemoteStorage { userAddress, token }) ->
                    [ ( "data", json )
                    , ( "file", file )
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
                    , ( "file", file )
                    ]
                        |> Json.object
                        |> Alien.broadcast Alien.AuthTextile
                        |> Ports.toTextile
                        |> Return.commandWithModel model

                -- ✋
                Nothing ->
                    return model


retrieveAllHypaethralData : Model -> Return Model Msg Reply
retrieveAllHypaethralData model =
    let
        maybeZipper =
            hypaethralBit.list
                |> List.map (\( _, b ) -> ( b, Json.null ))
                |> Zipper.fromList
    in
    case maybeZipper of
        Just zipper ->
            update
                (RetrieveHypaethralData <| Tuple.first <| Zipper.current zipper)
                { model | hypaethralRetrieval = Just zipper }

        Nothing ->
            return
                { model | hypaethralRetrieval = Nothing }



-- TERMINATION


type Termination
    = Authenticated Method Json.Value
    | NotAuthenticated


terminate : Termination -> List Reply
terminate t =
    case t of
        Authenticated method hypData ->
            [ GiveUI Alien.LoadHypaethralUserData hypData
            , GiveUI Alien.AuthMethod (encodeMethod method)
            ]

        NotAuthenticated ->
            [ NudgeUI Alien.NotAuthenticated
            , NudgeUI Alien.HideLoadingScreen
            ]
