module Brain exposing (main)

import Alien
import Brain.Authentication as Authentication
import Brain.Core exposing (..)
import Brain.Ports
import Brain.Reply as Reply exposing (Reply(..))
import Brain.Sources.Processing as Processing
import Brain.Sources.Processing.Common as Processing
import Json.Decode
import Replying exposing (return)
import Sources.Processing.Encoding as Processing



-- 🧠


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- 🌳


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { authentication = Authentication.initialModel
      , processing = Processing.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.batch
        [ Cmd.map AuthenticationMsg Authentication.initialCommand
        , Cmd.map ProcessingMsg Processing.initialCommand
        ]
    )



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            ( model
            , Cmd.none
            )

        NotifyUI alienEvent ->
            ( model
            , Brain.Ports.toUI alienEvent
            )

        -----------------------------------------
        -- Children
        -----------------------------------------
        AuthenticationMsg sub ->
            updateChild
                { mapCmd = AuthenticationMsg
                , mapModel = \child -> { model | authentication = child }
                , update = Authentication.update
                }
                { model = model.authentication
                , msg = sub
                }

        ProcessingMsg sub ->
            updateChild
                { mapCmd = ProcessingMsg
                , mapModel = \child -> { model | processing = child }
                , update = Processing.update
                }
                { model = model.processing
                , msg = sub
                }



-- 📣 ▒▒▒ CHILDREN & REPLIES


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        Chill ->
            Bypass

        -----------------------------------------
        -- To UI
        -----------------------------------------
        GiveUI tag data ->
            NotifyUI (Alien.broadcast tag data)

        NudgeUI tag ->
            NotifyUI (Alien.trigger tag)


updateChild =
    Replying.updateChild update translateReply



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Brain.Ports.fromCache translateAlienEvent
        , Brain.Ports.fromUI translateAlienEvent

        -----------------------------------------
        -- Children
        -----------------------------------------
        , Sub.map ProcessingMsg (Processing.subscriptions model.processing)
        ]


translateAlienEvent : Alien.Event -> Msg
translateAlienEvent event =
    case Alien.tagFromString event.tag of
        Just Alien.AuthAnonymous ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved event.data)

        Just Alien.AuthMethod ->
            AuthenticationMsg (Authentication.MethodRetrieved event.data)

        Just Alien.ProcessSources ->
            -- Only proceed to the processing if we got all the necessary data,
            -- otherwise report an error in the UI.
            case Json.Decode.decodeValue Processing.argumentsDecoder event.data of
                Ok arguments ->
                    arguments
                        |> Processing.Process
                        |> ProcessingMsg

                Err error ->
                    error
                        |> Json.Decode.errorToString
                        |> Alien.report Alien.ReportGenericError
                        |> NotifyUI

        Just Alien.SaveEnclosedUserData ->
            AuthenticationMsg (Authentication.SaveEnclosedData event.data)

        Just Alien.SaveHypaethralUserData ->
            AuthenticationMsg (Authentication.SaveHypaethralData event.data)

        Just Alien.SignIn ->
            AuthenticationMsg (Authentication.PerformSignIn event.data)

        Just Alien.SignOut ->
            AuthenticationMsg Authentication.PerformSignOut

        _ ->
            Bypass
