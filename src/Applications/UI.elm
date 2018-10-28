module UI exposing (main)

import Alien
import Authentication
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color
import Html exposing (Html, div, section)
import Html.Attributes exposing (style)
import Html.Events
import Json.Encode
import Replying exposing (return)
import Return2
import Return3
import Svg.Elements
import Tachyons.Classes as T
import UI.Authentication
import UI.Backdrop
import UI.Core exposing (Flags, Model, Msg(..), Switch(..))
import UI.Kit
import UI.Navigation
import UI.Page as Page
import UI.Ports as Ports
import UI.Reply as Reply exposing (Reply(..))
import Url exposing (Url)



-- ⛩


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- 🌳


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { isAuthenticated = False
      , isLoading = True
      , navKey = key
      , page = Page.fromUrl url
      , url = url

      -- Children
      , backdrop = UI.Backdrop.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.none
    )



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            ( model
            , Cmd.none
            )

        LoadUserData json ->
            ( { model | isAuthenticated = True, isLoading = False }
            , Cmd.none
            )

        ToggleLoadingScreen On ->
            ( { model | isLoading = True }
            , Cmd.none
            )

        ToggleLoadingScreen Off ->
            ( { model | isLoading = False }
            , Cmd.none
            )

        -----------------------------------------
        -- Children
        -----------------------------------------
        BackdropMsg sub ->
            updateChild
                { mapCmd = BackdropMsg
                , mapModel = \child -> { model | backdrop = child }
                , update = UI.Backdrop.update
                }
                { model = model.backdrop
                , msg = sub
                }

        -----------------------------------------
        -- Brain
        -----------------------------------------
        SignIn method ->
            ( model
            , method
                |> Authentication.methodToString
                |> Json.Encode.string
                |> Alien.broadcast Alien.SignIn
                |> Ports.toBrain
            )

        SignOut ->
            ( { model | isAuthenticated = False }
            , Alien.SignOut
                |> Alien.trigger
                |> Ports.toBrain
            )

        -----------------------------------------
        -- URL
        -----------------------------------------
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path == "/about" then
                        return model (Nav.load "/about")

                    else
                        return model (Nav.pushUrl model.navKey <| Url.toString url)

                Browser.External href ->
                    return model (Nav.load href)

        UrlChanged url ->
            ( { model
                | page = Page.fromUrl url
                , url = url
              }
            , Cmd.none
            )



-- 📣  ~  Children & Replies


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        Chill ->
            Bypass


updateChild =
    Replying.updateChild update translateReply



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fromBrain translateAlienEvent


translateAlienEvent : Alien.Event -> Msg
translateAlienEvent event =
    case Alien.tagFromString event.tag of
        Just Alien.HideLoadingScreen ->
            ToggleLoadingScreen Off

        Just Alien.LoadUserData ->
            LoadUserData event.data

        _ ->
            Bypass



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ root model ]
    }


root : Model -> Html Msg
root model =
    section
        [ style "color" (Color.toCssString UI.Kit.colors.text) ]
        [ -----------------------------------------
          -- Backdrop
          -----------------------------------------
          Html.map BackdropMsg (UI.Backdrop.view model.backdrop)

        -----------------------------------------
        -- Content
        -----------------------------------------
        , chunk
            [ T.flex
            , T.flex_column
            , T.items_center
            , T.justify_center
            , T.min_vh_100
            , T.relative
            , T.z_1
            ]
            (if model.isLoading then
                [ Html.map never Svg.Elements.spinner ]

             else if model.isAuthenticated then
                defaultScreen model

             else
                [ UI.Authentication.signInScreen ]
            )
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ UI.Navigation.global
        [ ( Page.Index, "Tracks" )
        ]
        model.page

    -- Main
    -- TODO
    , chunk
        [ T.bg_white
        , T.br1
        , T.flex_grow_1
        , T.pa3
        ]
        []

    -- Controls
    -- TODO
    , chunk
        [ T.h4 ]
        []
    ]
