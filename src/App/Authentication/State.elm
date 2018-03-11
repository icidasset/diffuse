module Authentication.State exposing (..)

import Authentication.Events exposing (..)
import Authentication.Translations as Translations
import Authentication.Ports as Ports
import Authentication.Types exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Navigation
import Response.Ext exposing (do)
import Routing.Types exposing (Page(Index))
import Settings.Types as Settings
import Types as TopLevel exposing (AlienEvent)
import Utils exposing (displayError, displayMessage)


-- 💧


initialModel : Model
initialModel =
    { method = Nothing
    , signedIn = False
    }


initialCommand : Cmd TopLevel.Msg
initialCommand =
    Cmd.batch
        [ issue MethodGet ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Step 1
        ------------------------------------
        Extraterrestrial MethodGet (Ok result) ->
            case decodeValue (maybe string) result of
                -- 🚀
                --
                Ok (Just m) ->
                    (!)
                        { model | method = Just (Translations.stringToMethod m) }
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
        Extraterrestrial MethodGet (Err err) ->
            handleError model err

        ------------------------------------
        -- Step 2
        ------------------------------------
        Extraterrestrial Construct (Ok _) ->
            (!) model [ issue IsSignedIn ]

        Extraterrestrial Construct (Err _) ->
            handleError model "Your browser does not support this authentication method"

        ------------------------------------
        -- Step 3
        ------------------------------------
        Extraterrestrial IsSignedIn (Ok result) ->
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
        Extraterrestrial IsSignedIn (Err err) ->
            handleError model err

        ------------------------------------
        -- Step 4a
        ------------------------------------
        Extraterrestrial GetData (Ok result) ->
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
        Extraterrestrial GetData (Err _) ->
            handleError
                model
                "Failed to retrieve the data from the selected authentication method"

        ------------------------------------
        -- Step 4b
        ------------------------------------
        Extraterrestrial IsSigningIn (Ok result) ->
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
        Extraterrestrial IsSigningIn (Err err) ->
            handleError model err

        ------------------------------------
        -- Step 4b.1
        ------------------------------------
        Extraterrestrial HandleSignInProcess (Ok result) ->
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
        Extraterrestrial HandleSignInProcess (Err err) ->
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
                    |> Utils.messageToString
                    |> Encode.string
                    |> issueWithData MethodSet
                ]

        ------------------------------------
        -- Sign in / Events
        ------------------------------------
        Extraterrestrial MethodSet (Ok _) ->
            (!) model [ issue SignIn ]

        Extraterrestrial MethodSet (Err err) ->
            handleError model err

        Extraterrestrial SignIn (Ok result) ->
            -- Handle SignIn consequence
            case decodeValue string result of
                Ok "GoBack" ->
                    (!)
                        model
                        [ afterwards ]

                Ok "None" ->
                    (!)
                        { model | signedIn = True }
                        [ issue Construct ]

                Ok "Redirect" ->
                    (!)
                        model
                        [ displayMessage "Redirecting ...\nA new tab or window might open."
                        , afterwards
                        ]

                Ok _ ->
                    (!)
                        model
                        [ afterwards
                        , displayError "Invalid response from SignIn"
                        , issue MethodUnset
                        ]

                Err err ->
                    (!)
                        model
                        [ afterwards
                        , displayError err
                        , issue MethodUnset
                        ]

        Extraterrestrial SignIn (Err err) ->
            (!)
                model
                [ afterwards
                , displayError err
                , issue MethodUnset
                ]

        ------------------------------------
        -- Sign out
        ------------------------------------
        PerformSignOut ->
            (!)
                { model | signedIn = False }
                [ do TopLevel.ShowLoadingScreen
                , do (TopLevel.ActiveQueueItemChanged Nothing)
                , do (TopLevel.RoutingMsg <| Routing.Types.GoToPage Index)
                , issue SignOut
                ]

        ------------------------------------
        -- Sign out / Events
        ------------------------------------
        Extraterrestrial SignOut (Ok _) ->
            (!) model [ issue Deconstruct ]

        Extraterrestrial SignOut (Err err) ->
            handleError model err

        Extraterrestrial Deconstruct (Ok _) ->
            (!) model [ issue MethodUnset ]

        Extraterrestrial Deconstruct (Err err) ->
            handleError model err

        Extraterrestrial MethodUnset (Ok _) ->
            (!) { model | method = Nothing } [ do TopLevel.Reset ]

        Extraterrestrial MethodUnset (Err err) ->
            handleError model err

        ------------------------------------
        -- Data storage
        ------------------------------------
        Extraterrestrial StoreData (Ok _) ->
            (!) model []

        Extraterrestrial StoreData (Err err) ->
            handleError model ("Data storage error: " ++ err)



-- 🔥 / Utilities


afterwards : Cmd TopLevel.Msg
afterwards =
    Cmd.batch
        [ do TopLevel.HideLoadingScreen
        , do (TopLevel.SettingsMsg Settings.SetDefaultBackdropIfNecessary)
        ]


handleError : Model -> String -> ( Model, Cmd TopLevel.Msg )
handleError model error =
    (!)
        model
        [ afterwards
        , displayError error
        ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.authenticationEventResult handleAlienEvent ]


handleAlienEvent : AlienEvent -> Msg
handleAlienEvent event =
    Extraterrestrial
        (Translations.stringToAlienMessage event.tag)
        (case event.error of
            Just err ->
                Err err

            Nothing ->
                Ok event.data
        )
