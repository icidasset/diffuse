module Brain exposing (main)

import Alien
import Brain.Authentication
import Brain.Core exposing (..)
import Brain.Ports
import Brain.Reply as Reply exposing (Reply(..))
import Json.Decode
import Replying exposing (return)



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
      { authentication = Brain.Authentication.initialModel }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.batch
        [ Cmd.map AuthenticationMsg Brain.Authentication.initialCommand ]
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



-- 📣  ~  Children & Replies


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        Chill ->
            Bypass

        HideLoadingScreen ->
            Alien.HideLoadingScreen
                |> Alien.trigger
                |> NotifyUI

        LoadUserData data ->
            data
                |> Alien.broadcast Alien.LoadUserData
                |> NotifyUI


updateChild =
    Replying.updateChild update translateReply



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Brain.Ports.fromCache translateAlienEvent
        , Brain.Ports.fromUI translateAlienEvent
        ]


translateAlienEvent : Alien.Event -> Msg
translateAlienEvent event =
    case Alien.tagFromString event.tag of
        Just Alien.AuthAnonymous ->
            AuthenticationMsg (Brain.Authentication.DataRetrieved event.data)

        Just Alien.AuthMethod ->
            AuthenticationMsg (Brain.Authentication.MethodRetrieved event.data)

        Just Alien.SignIn ->
            AuthenticationMsg (Brain.Authentication.PerformSignIn event.data)

        Just Alien.SignOut ->
            AuthenticationMsg Brain.Authentication.PerformSignOut

        _ ->
            Bypass
