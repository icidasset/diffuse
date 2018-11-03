module UI exposing (main)

import Alien
import Authentication
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color
import Html exposing (Html, div, section, text)
import Html.Attributes exposing (style)
import Html.Lazy
import Json.Encode
import Replying exposing (return)
import Return2
import Return3
import Sources
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
import UI.Settings
import UI.Sources
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
      , sources = UI.Sources.initialModel
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

        SourcesMsg sub ->
            updateChild
                { mapCmd = SourcesMsg
                , mapModel = \child -> { model | sources = child }
                , update = UI.Sources.update
                }
                { model = model.sources
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
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    root
        [ -----------------------------------------
          -- Backdrop
          -----------------------------------------
          model.backdrop
            |> Html.Lazy.lazy UI.Backdrop.view
            |> Html.map BackdropMsg

        -----------------------------------------
        -- Content
        -----------------------------------------
        , content
            (if model.isLoading then
                [ spinner ]

             else if model.isAuthenticated then
                defaultScreen model

             else
                [ UI.Authentication.signInScreen ]
            )
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ Html.Lazy.lazy
        (UI.Navigation.global
            [ ( Page.Index, "Tracks" )
            , ( Page.Sources Sources.Index, "Sources" )
            , ( Page.Settings, "Settings" )
            ]
        )
        model.page

    -----------------------------------------
    -- Main
    -----------------------------------------
    , case model.page of
        Page.Index ->
            -- TODO: Tracks
            empty

        Page.NotFound ->
            -- TODO
            text "Page not found."

        Page.Settings ->
            UI.Settings.view model

        Page.Sources subPage ->
            model.sources
                |> Html.Lazy.lazy2 UI.Sources.view subPage
                |> Html.map SourcesMsg

    -----------------------------------------
    -- Controls
    -----------------------------------------
    -- TODO
    , chunk
        [ T.h4 ]
        []
    ]



-- 🗺  ~  Bits


root : List (Html msg) -> Html msg
root =
    section
        [ style "color" (Color.toCssString UI.Kit.colors.text) ]


content : List (Html msg) -> Html msg
content =
    chunk
        [ T.flex
        , T.flex_column
        , T.items_center
        , T.justify_center
        , T.min_vh_100
        , T.ph3
        , T.relative
        , T.z_1
        ]


spinner : Html msg
spinner =
    Html.map never Svg.Elements.spinner
