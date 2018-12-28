module Brain exposing (main)

import Alien
import Brain.Authentication
import Brain.Core exposing (..)
import Brain.Ports
import Brain.Reply as Reply exposing (Reply(..))
import Brain.Sources.Processing
import Json.Decode
import Replying exposing (return)
import Sources.Processing.Encoding



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
      { authentication = Brain.Authentication.initialModel
      , sourceProcessing = Brain.Sources.Processing.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.batch
        [ Cmd.map AuthenticationMsg Brain.Authentication.initialCommand
        , Cmd.map SourceProcessingMsg Brain.Sources.Processing.initialCommand
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
                , update = Brain.Authentication.update
                }
                { model = model.authentication
                , msg = sub
                }

        SourceProcessingMsg sub ->
            updateChild
                { mapCmd = SourceProcessingMsg
                , mapModel = \child -> { model | sourceProcessing = child }
                , update = Brain.Sources.Processing.update
                }
                { model = model.sourceProcessing
                , msg = sub
                }



-- 📣  |  Children & Replies


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        Chill ->
            Bypass

        -----------------------------------------
        -- To UI
        -----------------------------------------
        HideLoadingScreen ->
            Alien.HideLoadingScreen
                |> Alien.trigger
                |> NotifyUI

        LoadEnclosedUserData data ->
            NotifyUI (Alien.broadcast Alien.LoadEnclosedUserData data)

        LoadHypaethralUserData data ->
            NotifyUI (Alien.broadcast Alien.LoadHypaethralUserData data)

        ReportSourceProcessingError data ->
            NotifyUI (Alien.broadcast Alien.ReportSourceProcessingError data)


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
        , Sub.map
            SourceProcessingMsg
            (Brain.Sources.Processing.subscriptions model.sourceProcessing)
        ]


translateAlienEvent : Alien.Event -> Msg
translateAlienEvent event =
    case Alien.tagFromString event.tag of
        Just Alien.AuthAnonymous ->
            AuthenticationMsg (Brain.Authentication.HypaethralDataRetrieved event.data)

        Just Alien.AuthMethod ->
            AuthenticationMsg (Brain.Authentication.MethodRetrieved event.data)

        Just Alien.ProcessSources ->
            -- Only proceed to the processing if we got all the necessary data,
            -- otherwise report an error in the UI.
            case Json.Decode.decodeValue Sources.Processing.Encoding.argumentsDecoder event.data of
                Ok arguments ->
                    SourceProcessingMsg (Brain.Sources.Processing.Process arguments)

                Err error ->
                    error
                        |> Json.Decode.errorToString
                        |> Alien.report Alien.ReportGenericError
                        |> NotifyUI

        Just Alien.SaveEnclosedUserData ->
            AuthenticationMsg (Brain.Authentication.SaveEnclosedData event.data)

        Just Alien.SaveHypaethralUserData ->
            AuthenticationMsg (Brain.Authentication.SaveHypaethralData event.data)

        Just Alien.SignIn ->
            AuthenticationMsg (Brain.Authentication.PerformSignIn event.data)

        Just Alien.SignOut ->
            AuthenticationMsg Brain.Authentication.PerformSignOut

        _ ->
            Bypass
