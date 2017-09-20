module Authentication.State exposing (..)

import Authentication.Method
import Authentication.Types exposing (..)
import Dict
import Equalizer.State as Equalizer
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import Queue.Ports
import Response.Ext exposing (do, doDelayed)
import Routing.Types exposing (Page(..))
import Task exposing (Task)
import Time
import Tracks.Encoding
import Tracks.Ports
import Tracks.Types
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
        [ Task.attempt
            (DidGetMethod >> TopLevel.AuthenticationMsg)
            (Authentication.Method.get)
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Step 1
        ------------------------------------
        DidGetMethod (Ok m) ->
            (!)
                { model | method = m }
                [ case m of
                    Just method ->
                        Task.attempt
                            (DidConstruct >> TopLevel.AuthenticationMsg)
                            (Authentication.Method.construct method)

                    Nothing ->
                        after
                ]

        DidGetMethod (Err _) ->
            -- Do nothing, the user has not used the application before.
            -- So they haven't signed in yet.
            (!)
                model
                [ after ]

        ------------------------------------
        -- Step 2
        ------------------------------------
        DidConstruct (Ok _) ->
            (!)
                model
                [ case model.method of
                    Just method ->
                        Task.perform
                            (DidGetIsSignedIn >> TopLevel.AuthenticationMsg)
                            (Authentication.Method.isSignedIn method)

                    Nothing ->
                        after
                ]

        DidConstruct (Err e) ->
            (!)
                model
                [ "Your browser does not support this authentication method"
                    |> ErrorScreen
                    |> Routing.Types.SetPage
                    |> TopLevel.RoutingMsg
                    |> do
                ]

        ------------------------------------
        -- Step 3
        ------------------------------------
        DidGetIsSignedIn isSignedIn ->
            (!)
                { model | signedIn = isSignedIn }
                [ case model.method of
                    Just method ->
                        if isSignedIn then
                            Task.attempt
                                (DidGetData >> TopLevel.AuthenticationMsg)
                                (Authentication.Method.getData method)
                        else
                            Task.perform
                                (DidGetIsSigningIn >> TopLevel.AuthenticationMsg)
                                (Authentication.Method.isSigningIn method)

                    Nothing ->
                        after
                ]

        ------------------------------------
        -- Step 4a
        ------------------------------------
        DidGetData (Ok (Just json)) ->
            (!) model [ do (TopLevel.ImportUserData json) ]

        DidGetData (Ok Nothing) ->
            -- The user has no data yet,
            -- so nothing to do here.
            (!) model [ after ]

        DidGetData (Err _) ->
            (!)
                model
                [ "Failed to retrieve the data from the selected authentication method"
                    |> ErrorScreen
                    |> Routing.Types.SetPage
                    |> TopLevel.RoutingMsg
                    |> do
                ]

        ------------------------------------
        -- Step 4b
        ------------------------------------
        DidGetIsSigningIn True ->
            (!)
                model
                [ case model.method of
                    Just method ->
                        Task.perform
                            (DidHandleSignInProcess >> TopLevel.AuthenticationMsg)
                            (Authentication.Method.handleSignInProcess method)

                    Nothing ->
                        after
                ]

        DidGetIsSigningIn False ->
            (!)
                model
                [ after ]

        DidHandleSignInProcess consequence ->
            (!)
                { model | signedIn = True }
                [ case consequence of
                    KeepUrl ->
                        Cmd.none

                    ModifyUrl ->
                        Navigation.modifyUrl "/"
                , case model.method of
                    Just method ->
                        Task.attempt
                            (DidGetData >> TopLevel.AuthenticationMsg)
                            (Authentication.Method.getData method)

                    Nothing ->
                        after
                ]

        ------------------------------------
        -- Sign in
        ------------------------------------
        SignIn method ->
            (!)
                { model | method = Just method }
                [ Task.attempt
                    (always TopLevel.NoOp)
                    (Authentication.Method.set method)

                -- Sign in
                , Task.perform
                    (DidSignIn >> TopLevel.AuthenticationMsg)
                    (Authentication.Method.signIn method)

                -- Show loading screen
                , TopLevel.ShowLoadingScreen
                    |> do
                ]

        DidSignIn consequence ->
            case consequence of
                None ->
                    (!)
                        { model | signedIn = True }
                        [ case model.method of
                            Just method ->
                                Task.attempt
                                    (DidConstruct >> TopLevel.AuthenticationMsg)
                                    (Authentication.Method.construct method)

                            Nothing ->
                                Cmd.none
                        ]

                Redirect ->
                    (!) model []

        ------------------------------------
        -- Sign out
        ------------------------------------
        SignOut ->
            (!)
                { model | method = Nothing, signedIn = False }
                [ Task.attempt
                    (always TopLevel.NoOp)
                    (Authentication.Method.remove)
                , case model.method of
                    Just method ->
                        Task.attempt
                            (always TopLevel.NoOp)
                            (Authentication.Method.deconstruct method)

                    Nothing ->
                        Cmd.none

                -- Clear all data
                , do TopLevel.Reset
                ]


after : Cmd TopLevel.Msg
after =
    do TopLevel.HideLoadingScreen
