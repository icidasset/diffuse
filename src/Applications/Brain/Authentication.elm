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
    2. Get unrestricted data

-}

import Alien
import Authentication exposing (..)
import Brain.Ports as Ports
import Brain.Reply exposing (Reply(..))
import Json.Decode as J
import Json.Encode
import Replying exposing (R3D3, do)



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
                Just Local ->
                    Ports.requestCache (Alien.trigger Alien.AuthAnonymous)

                -- ✋
                Nothing ->
                    Cmd.none
            , Nothing
            )

        HypaethralDataRetrieved json ->
            ( model
            , noCmd
            , terminate (Authenticated json)
            )

        -----------------------------------------
        -- DATA
        -----------------------------------------
        RetrieveEnclosedData ->
            ( model
            , Ports.requestCache (Alien.trigger Alien.AuthEnclosedData)
            , Nothing
            )

        EnclosedDataRetrieved json ->
            ( model
            , noCmd
            , Just [ LoadEnclosedUserData json ]
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
                Just Local ->
                    Ports.toCache (Alien.broadcast Alien.AuthAnonymous json)

                -- ✋
                Nothing ->
                    Cmd.none
            , Nothing
            )


noCmd : Cmd msg
noCmd =
    Cmd.none



-- JSON


decodeMethod : J.Value -> Maybe Method
decodeMethod json =
    json
        |> J.decodeValue J.string
        |> Result.toMaybe
        |> Maybe.andThen Authentication.methodFromString



-- TERMINATION


type Termination
    = Authenticated J.Value
    | NotAuthenticated


terminate : Termination -> Maybe (List Reply)
terminate t =
    case t of
        Authenticated json ->
            Just [ LoadHypaethralUserData json ]

        NotAuthenticated ->
            Just [ HideLoadingScreen ]
