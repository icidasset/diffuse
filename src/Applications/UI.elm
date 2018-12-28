module UI exposing (main)

import Alien
import Authentication
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Css exposing (url)
import Css.Global
import Html.Styled as Html exposing (Html, div, section, text, toUnstyled)
import Html.Styled.Attributes exposing (id, style)
import Html.Styled.Lazy as Lazy
import Json.Encode
import Replying exposing (return)
import Return2
import Return3
import Sources
import Tachyons.Classes as T
import UI.Authentication
import UI.Backdrop
import UI.Core as Core exposing (Flags, Model, Msg(..), Switch(..))
import UI.Kit
import UI.Navigation
import UI.Page as Page
import UI.Ports as Ports
import UI.Reply as Reply exposing (Reply(..))
import UI.Settings
import UI.Sources
import UI.Svg.Elements
import UI.UserData
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

        LoadEnclosedUserData json ->
            ( model
            , Cmd.none
            )

        LoadHypaethralUserData json ->
            ( { model | isAuthenticated = True, isLoading = False }
                |> UI.UserData.importHypaethral json
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
        NotifyBrain alienEvent ->
            ( model
            , Ports.toBrain alienEvent
            )

        Core.SaveEnclosedUserData ->
            ( model
            , Cmd.none
            )

        Core.SaveHypaethralUserData ->
            ( model
            , model
                |> UI.UserData.exportHypaethral
                |> Alien.broadcast Alien.SaveHypaethralUserData
                |> Ports.toBrain
            )

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
        ChangeUrlUsingPage page ->
            ( model
            , Nav.pushUrl model.navKey (Page.toString page)
            )

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
            , Ports.removeFocus ()
            )



-- 📣  |  Children & Replies


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        AddSourceToCollection source ->
            SourcesMsg (UI.Sources.AddToCollection source)

        Chill ->
            Bypass

        GoToPage page ->
            ChangeUrlUsingPage page

        Reply.SaveEnclosedUserData ->
            Core.SaveEnclosedUserData

        Reply.SaveHypaethralUserData ->
            Core.SaveHypaethralUserData


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

        Just Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData event.data

        Just Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData event.data

        _ ->
            Bypass



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ toUnstyled (body model) ]
    }


body : Model -> Html Msg
body model =
    section
        []
        [ Css.Global.global globalCss

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , model.backdrop
            |> Lazy.lazy UI.Backdrop.view
            |> Html.map BackdropMsg

        -----------------------------------------
        -- Content
        -----------------------------------------
        , content
            (if model.isLoading then
                [ loadingAnimation ]

             else if model.isAuthenticated then
                defaultScreen model

             else
                [ UI.Authentication.signInScreen ]
            )
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ Lazy.lazy
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
            empty

        Page.NotFound ->
            text "Page not found."

        Page.Settings ->
            UI.Settings.view model

        Page.Sources subPage ->
            model.sources
                |> Lazy.lazy2 UI.Sources.view subPage
                |> Html.map SourcesMsg

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , chunk
        [ T.h4 ]
        []
    ]



-- 🗺  |  Bits


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


loadingAnimation : Html msg
loadingAnimation =
    Html.map never UI.Svg.Elements.loading



-- 🖼  |  Global


globalCss : List Css.Global.Snippet
globalCss =
    [ -----------------------------------------
      -- Body
      -----------------------------------------
      Css.Global.body
        [ Css.color (Color.toElmCssColor UI.Kit.colors.text)
        , Css.fontFamilies UI.Kit.defaultFontFamilies
        , Css.textRendering Css.optimizeLegibility

        -- Font smoothing
        -----------------
        , Css.property "-webkit-font-smoothing" "antialiased"
        , Css.property "-moz-osx-font-smoothing" "grayscale"
        , Css.property "font-smoothing" "antialiased"
        ]

    -----------------------------------------
    -- Placeholders
    -----------------------------------------
    , Css.Global.selector "::-webkit-input-placeholder" placeholderStyles
    , Css.Global.selector "::-moz-placeholder" placeholderStyles
    , Css.Global.selector ":-ms-input-placeholder" placeholderStyles
    , Css.Global.selector ":-moz-placeholder" placeholderStyles
    , Css.Global.selector "::placeholder" placeholderStyles
    ]


placeholderStyles : List Css.Style
placeholderStyles =
    [ Css.color (Css.rgb 0 0 0)
    , Css.opacity (Css.num 0.2)
    ]
