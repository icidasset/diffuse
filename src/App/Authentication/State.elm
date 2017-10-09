module Authentication.State exposing (..)

import Authentication.Transformers as Transformers
import Authentication.Ports as Ports
import Authentication.Types exposing (..)
import Dict
import Equalizer.State as Equalizer
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Navigation
import Queue.Ports
import Response.Ext exposing (do)
import Routing.Types exposing (Page(..))
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { method = Nothing
    , signedIn = False
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.batch
        [ issue MethodGet ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Step 1
        ------------------------------------
        Incoming MethodGet (Ok result) ->
            case decodeValue (maybe string) result of
                -- 🚀
                --
                Ok (Just m) ->
                    (!)
                        { model | method = Just (Transformers.stringToMethod m) }
                        [ issue Construct ]

                -- No method set,
                -- carry on.
                Ok Nothing ->
                    (!)
                        model
                        [ afterwards ]

                -- ⚠️
                --
                Err err ->
                    handleError model err

        --
        -- Error
        --
        Incoming MethodGet (Err err) ->
            handleError model err

        ------------------------------------
        -- Step 2
        ------------------------------------
        Incoming Construct (Ok _) ->
            (!) model [ issue IsSignedIn ]

        Incoming Construct (Err _) ->
            handleError model "Your browser does not support this authentication method"

        ------------------------------------
        -- Step 3
        ------------------------------------
        Incoming IsSignedIn (Ok result) ->
            case decodeValue bool result of
                -- Yes
                --
                Ok True ->
                    (!) { model | signedIn = True } [ issue GetData ]

                -- No
                --
                Ok False ->
                    (!) { model | signedIn = False } [ issue IsSigningIn ]

                -- ?
                --
                Err err ->
                    handleError model err

        --
        -- Error
        --
        Incoming IsSignedIn (Err err) ->
            handleError model err

        ------------------------------------
        -- Step 4a
        ------------------------------------
        Incoming GetData (Ok result) ->
            (!)
                model
                [ case decodeValue (maybe string) result of
                    Ok (Just json) ->
                        do (TopLevel.ImportUserData json)

                    Ok Nothing ->
                        afterwards

                    Err err ->
                        displayError err
                ]

        --
        -- Error
        --
        Incoming GetData (Err _) ->
            handleError
                model
                "Failed to retrieve the data from the selected authentication method"

        ------------------------------------
        -- Step 4b
        ------------------------------------
        Incoming IsSigningIn (Ok result) ->
            (!)
                model
                [ case decodeValue bool result of
                    Ok True ->
                        issue HandleSignInProcess

                    Ok False ->
                        afterwards

                    Err err ->
                        displayError err
                ]

        --
        -- Error
        --
        Incoming IsSigningIn (Err err) ->
            handleError model err

        ------------------------------------
        -- Step 4b.1
        ------------------------------------
        Incoming HandleSignInProcess (Ok result) ->
            (!)
                { model | signedIn = True }
                [ issue GetData
                , case decodeValue string result of
                    Ok "KeepUrl" ->
                        Cmd.none

                    Ok "ModifyUrl" ->
                        Navigation.modifyUrl "/"

                    Ok _ ->
                        displayError "Invalid response from HandleSignInProcess"

                    Err err ->
                        displayError err
                ]

        --
        -- Error
        --
        Incoming HandleSignInProcess (Err err) ->
            handleError model err

        ------------------------------------
        -- Sign in
        ------------------------------------
        PerformSignIn method ->
            (!)
                { model | method = Just method }
                [ do TopLevel.ShowLoadingScreen

                -- Set method
                , method
                    |> Transformers.methodToString
                    |> Encode.string
                    |> issueWithData MethodSet
                ]

        ------------------------------------
        -- Sign in / Events
        ------------------------------------
        Incoming MethodSet (Ok _) ->
            (!) model [ issue SignIn ]

        Incoming MethodSet (Err err) ->
            handleError model err

        Incoming SignIn (Ok result) ->
            -- Handle SignIn consequence
            case decodeValue string result of
                Ok "None" ->
                    (!)
                        { model | signedIn = True }
                        [ issue Construct ]

                Ok "Redirect" ->
                    (!)
                        model
                        []

                Ok _ ->
                    handleError model "Invalid response from SignIn"

                Err err ->
                    handleError model err

        Incoming SignIn (Err err) ->
            handleError model err

        ------------------------------------
        -- Sign out
        ------------------------------------
        PerformSignOut ->
            (!)
                { model | signedIn = False }
                [ do TopLevel.ShowLoadingScreen
                , issue SignOut
                ]

        ------------------------------------
        -- Sign out / Events
        ------------------------------------
        Incoming SignOut (Ok _) ->
            (!) model [ issue Deconstruct ]

        Incoming SignOut (Err err) ->
            handleError model err

        Incoming Deconstruct (Ok _) ->
            (!) model [ issue MethodUnset ]

        Incoming Deconstruct (Err err) ->
            handleError model err

        Incoming MethodUnset (Ok _) ->
            (!) { model | method = Nothing } [ do TopLevel.Reset ]

        Incoming MethodUnset (Err err) ->
            handleError model err

        ------------------------------------
        -- Data storage
        ------------------------------------
        Incoming StoreData _ ->
            (!) model []



-- 🔥 / Utilities


afterwards : Cmd TopLevel.Msg
afterwards =
    do TopLevel.HideLoadingScreen


displayError : String -> Cmd TopLevel.Msg
displayError error =
    error
        |> ErrorScreen
        |> Routing.Types.SetPage
        |> TopLevel.RoutingMsg
        |> do


handleError : Model -> String -> ( Model, Cmd TopLevel.Msg )
handleError model error =
    (!)
        model
        [ afterwards
        , displayError error
        ]


issue : OutgoingMsg -> Cmd TopLevel.Msg
issue msg =
    issueWithData msg Encode.null


issueWithData : OutgoingMsg -> Encode.Value -> Cmd TopLevel.Msg
issueWithData msg data =
    Ports.authenticationEvent
        { tag = Transformers.outgoingMessageToString msg
        , data = data
        , error = Nothing
        }



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.authenticationEventResult toMessage
        ]


toMessage : OutsideEvent -> Msg
toMessage event =
    Incoming
        (Transformers.stringToOutgoingMessage event.tag)
        (case event.error of
            Just err ->
                Err err

            Nothing ->
                Ok event.data
        )
