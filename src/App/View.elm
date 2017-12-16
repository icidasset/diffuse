module View exposing (entry)

import Equalizer.Touch
import Html exposing (Html)
import Json.Decode as Decode
import Material.Icons.Action
import Material.Icons.Alert
import Maybe.Extra as Maybe
import Svg.Elements
import Types exposing (..)


-- Elements & Styles

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (on, onClick)
import Element.Ext as Element
import Styles exposing (..)
import Variables exposing (colors, colorDerivatives, insulationWidth, scaled)
import Variations exposing (Variations)


-- Notifications

import Notifications.Config
import Notifications.Types exposing (Notification)
import Toasty


-- Children

import Alfred.View as Alfred
import Abroad.View as Abroad
import Console.View as Console
import ContextMenu.View as ContextMenu
import Equalizer.View as Equalizer
import Navigation.View as Navigation
import Notifications.View as Notifications
import Playlists.View as Playlists
import Queue.View as Queue
import Settings.View as Settings
import Sources.View as Sources
import Tracks.View as Tracks


-- Children, Pt. 2

import Authentication.Types exposing (Method(..))
import Navigation.Types exposing (Icon(..))
import Routing.Types exposing (Page(..))
import Sources.Types


-- ⚗️


type alias InsulationOptions =
    { nested : Bool }



-- 🍯


entry : Model -> Html Msg
entry model =
    [ entryNodes model
    , entryLazyNodes model
    ]
        |> List.concat
        |> column Root (entryAttributes model)
        |> viewport Styles.styles


entryNodes : Model -> List Node
entryNodes model =
    if model.showLoadingScreen then
        ------------------------------------
        -- {1} Loading
        ------------------------------------
        [ loadingScreen
        ]
    else if requiresAuthentication model then
        ------------------------------------
        -- {2} Authentication is required
        ------------------------------------
        [ mainNav model
        , loginScreen model
        ]
    else
        ------------------------------------
        -- {3} Otherwise
        ------------------------------------
        [ mainNav model

        -- Main
        --
        , case model.routing.currentPage of
            ErrorScreen _ ->
                currentPage unnested model

            MessageScreen _ ->
                currentPage unnested model

            _ ->
                model
                    |> Tracks.entry
                    |> html
                    |> insulate unnested
                    |> within [ currentPage nested model ]

        -- Console
        --
        , if model.authentication.signedIn then
            html (Console.entry model)
          else
            empty
        ]


entryLazyNodes : Model -> List Node
entryLazyNodes model =
    [ Element.lazy alfred model.alfred
    , Element.lazy backgroundImage model.settings.backgroundImage
    , Element.lazy notifications model.toasties
    , Element.lazy2 overlay model.alfred model.contextMenu
    , Element.lazy ContextMenu.entry model.contextMenu
    ]


{-| Global mouse/touch events & Equalizer events.

TODO:
Find a way to make these into subscriptions.
Like we did with the mouse events.

-}
entryAttributes : Model -> List Attr
entryAttributes model =
    List.append
        -- Equalizer
        (if model.isTouchDevice && Maybe.isJust model.equalizer.activeKnob then
            [ on "touchmove" Equalizer.Touch.move
            , on "touchend" Equalizer.Touch.end
            ]
         else
            []
        )
        -- Layout
        [ height fill
        , paddingXY (scaled 2) 0

        -- Click away
        , if model.isTouchDevice then
            on "tap" (Decode.succeed ClickAway)
          else
            onClick ClickAway
        ]



-- 🐝


requiresAuthentication : Model -> Bool
requiresAuthentication model =
    case model.routing.currentPage of
        ErrorScreen _ ->
            False

        MessageScreen _ ->
            False

        _ ->
            not model.authentication.signedIn


currentPage : InsulationOptions -> Model -> Node
currentPage insulationOptions model =
    case model.routing.currentPage of
        ------------------------------------
        -- # Doesn't require authentication
        ------------------------------------
        ErrorScreen err ->
            errorScreen err

        MessageScreen message ->
            messageScreen message

        ------------------------------------
        -- # Requires authentication
        ------------------------------------
        Abroad ->
            model
                |> Abroad.entry
                |> html
                |> insulate insulationOptions

        Equalizer ->
            model
                |> Equalizer.entry
                |> html
                |> insulate insulationOptions

        Index ->
            empty

        Playlists playlistsPage ->
            model
                |> Playlists.entry playlistsPage
                |> html
                |> insulate insulationOptions

        Queue queuePage ->
            model
                |> Queue.entry queuePage
                |> html
                |> insulate insulationOptions

        Settings ->
            model
                |> Settings.entry
                |> html
                |> insulate insulationOptions

        Sources sourcePage ->
            model
                |> Sources.entry sourcePage
                |> html
                |> insulate insulationOptions



-- Screens


errorScreen : String -> Node
errorScreen err =
    [ html (Material.Icons.Alert.error Color.white 14)
    , bold err
    ]
        |> row Message [ center, spacingXY (scaled -2) 0, verticalCenter ]
        |> el Zed [ center, verticalCenter ]
        |> el Zed [ height fill ]


loadingScreen : Node
loadingScreen =
    Svg.Elements.spinner
        |> html
        |> el Zed [ center, verticalCenter ]
        |> el Zed [ height fill ]


loginScreen : Model -> Node
loginScreen model =
    el
        Zed
        [ height fill ]
        (el
            Zed
            [ center, verticalCenter ]
            (column
                AuthenticationOptions
                [ paddingXY (scaled 5) (scaled -8) ]
                [ authButton model.isHTTPS Blockstack
                , authButton model.isHTTPS RemoteStorage
                , authButton model.isHTTPS Local
                ]
            )
        )


messageScreen : String -> Node
messageScreen message =
    message
        |> String.split "\n"
        |> List.map italic
        |> column Message [ center ]
        |> el Zed [ center, verticalCenter ]
        |> el Zed [ height fill, paddingBottom (scaled 15) ]



-- Authentication


authButton : Bool -> Authentication.Types.Method -> Node
authButton isHTTPS authMethod =
    el
        AuthenticationButton
        [ minWidth (px 260)
        , paddingXY 0 (scaled -1)
        , spacing 1

        -- Events
        , authMethod
            |> Authentication.Types.PerformSignIn
            |> AuthenticationMsg
            |> onClick
        ]
        (row
            Zed
            [ spacing (scaled -2) ]
            (case authMethod of
                Local ->
                    authButtonInterior
                        ( Material.Icons.Action.lock colorDerivatives.text 17
                        , "Sign in anonymously"
                        )

                Blockstack ->
                    authButtonInterior
                        ( Svg.Elements.blockstackLogo
                        , "Sign in with Blockstack"
                        )

                RemoteStorage ->
                    authButtonInterior
                        ( Svg.Elements.remoteStorageLogo
                        , "Sign in with Remote Storage"
                        )
            )
        )


authButtonInterior : ( Html Msg, String ) -> List Node
authButtonInterior ( icon, label ) =
    icon
        |> html
        |> el Zed [ center ]
        |> el WithoutLineHeight [ moveUp 1, verticalCenter, width (px 22) ]
        |> List.singleton
        |> List.append [ text label ]
        |> List.reverse


authenticatedNavigation : Page -> Maybe Alfred -> Node
authenticatedNavigation currentPage maybeAlfred =
    let
        styles =
            case maybeAlfred of
                Just _ ->
                    [ ( "visibility", "hidden" ) ]

                Nothing ->
                    []
    in
        el
            Zed
            [ inlineStyle styles ]
            (Navigation.outside
                currentPage
                [ ( "Tracks", Routing.Types.Index )
                , ( "Sources", Routing.Types.Sources Sources.Types.Index )
                , ( "Settings", Routing.Types.Settings )
                ]
            )


unauthenticatedNavigation : Page -> Node
unauthenticatedNavigation currentPage =
    Navigation.outsideOutgoing
        currentPage
        [ ( Icon Material.Icons.Action.home, "/" )
        , ( Icon Material.Icons.Action.info, "/about" )
        ]



-- Insulation


insulate : InsulationOptions -> Node -> Node
insulate insulationOptions node =
    el
        (if insulationOptions.nested then
            NestedInsulation
         else
            Insulation
        )
        [ clip
        , center
        , height fill
        , maxWidth (px insulationWidth)
        , width fill
        ]
        node


nested : InsulationOptions
nested =
    { nested = True }


unnested : InsulationOptions
unnested =
    { nested = False }



-- Shared


alfred : Maybe Alfred -> Node
alfred maybe =
    case maybe of
        Just context ->
            html (Alfred.entry context)

        Nothing ->
            empty


backgroundImage : String -> Node
backgroundImage img =
    el Zed [ inlineStyle (backgroundImageStyles img) ] empty


backgroundImageStyles : String -> List ( String, String )
backgroundImageStyles img =
    [ ( "background-image", "url(/images/Background/" ++ img ++ ")" )
    , ( "background-size", "cover" )
    , ( "bottom", "-1px" )
    , ( "left", "-1px" )
    , ( "position", "fixed" )
    , ( "right", "-1px" )
    , ( "top", "-1px" )
    , ( "z-index", "-10" )

    --
    , case img of
        "1.jpg" ->
            ( "background-position", "center 30%" )

        _ ->
            ( "background-position", "center bottom" )
    ]


mainNav : Model -> Node
mainNav model =
    el
        Zed
        [ center
        , paddingBottom (scaled 12)
        , paddingTop (scaled 9)
        , width fill
        ]
        (if model.authentication.signedIn then
            Element.lazy2
                authenticatedNavigation
                model.routing.currentPage
                model.alfred
         else
            Element.lazy
                unauthenticatedNavigation
                model.routing.currentPage
        )


notifications : Toasty.Stack Notification -> Node
notifications stack =
    stack
        |> Toasty.view
            Notifications.Config.config
            Notifications.entry
            ToastyMsg
        |> html


overlay : Maybe Alfred -> Maybe ContextMenu -> Node
overlay a b =
    let
        additionalStyles =
            if Maybe.isJust a || Maybe.isJust b then
                [ ( "cursor", "pointer" ), ( "opacity", "1" ) ]
            else
                [ ( "pointer-events", "none" ) ]
    in
        el
            Overlay
            [ height fill
            , width fill
            , overlayPositioning
                |> List.append additionalStyles
                |> inlineStyle
            ]
            empty


overlayPositioning : List ( String, String )
overlayPositioning =
    [ ( "left", "0" )
    , ( "position", "fixed" )
    , ( "top", "0" )
    , ( "z-index", "900" )
    ]
