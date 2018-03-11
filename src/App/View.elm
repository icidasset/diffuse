module View exposing (entry)

import Equalizer.Touch
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
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
import Element.Input as Input
import Element.Types exposing (Attr, Node)
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
import Settings.Types
import Sources.Types


-- ⚗️


type alias InsulationOptions =
    { nested : Bool }



-- 🍯


entry : Model -> Html Msg
entry model =
    [ entryNodesWrapped model
    , entryLazyNodes model
    ]
        |> List.concat
        |> column Root (entryAttributes model)
        |> viewport Styles.styles


entryNodesWrapped : Model -> List Node
entryNodesWrapped model =
    [ column
        Zed
        [ height fill ]
        (entryNodes model)
    ]


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
        [ loginScreen model
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
                    |> insulate unnested
                    |> within [ currentPage nested model ]

        -- Console
        --
        , if model.authentication.signedIn then
            Console.entry model
          else
            empty
        ]


entryLazyNodes : Model -> List Node
entryLazyNodes model =
    let
        s =
            model.settings
    in
        [ Element.lazy alfred model.alfred
        , Element.lazy backdropChosen s.chosenBackdrop
        , Element.lazy2 backdropLoaded s.loadedBackdrops s.fadeInLastBackdrop
        , Element.lazy notifications model.toasties
        , Element.lazy2 overlay model.alfred model.contextMenu
        , Element.lazy ContextMenu.entry model.contextMenu
        ]


{-| Global mouse/touch events & Equalizer events.

TODO:
Find a way to make these equalizer events into subscriptions.
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
                |> insulate insulationOptions

        Routing.Types.Equalizer ->
            model
                |> Equalizer.entry
                |> insulate insulationOptions

        Index ->
            empty

        Playlists playlistsPage ->
            model
                |> Playlists.entry playlistsPage
                |> insulate insulationOptions

        Queue queuePage ->
            model
                |> Queue.entry queuePage
                |> insulate insulationOptions

        Settings ->
            model
                |> Settings.entry
                |> insulate insulationOptions

        Sources sourcePage ->
            model
                |> Sources.entry sourcePage
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
    column
        Zed
        [ height fill
        , center
        , verticalCenter
        ]
        [ el
            LogoFull
            [ height (percent 45)
            , width (px 190)
            ]
            empty

        --
        , column
            AuthenticationOptions
            [ paddingXY (scaled 5) (scaled -8) ]
            [ if not model.isElectron then
                authButton Blockstack
              else
                empty

            --
            , authButton RemoteStorage
            , authButton Local
            ]

        --
        , el Zed [ height fill ] empty

        --
        , let
            styles =
                [ ( "color", "white" )
                , ( "opacity", "0.5" )
                , ( "font-size", "0.9em" )
                ]
          in
            "What is this exactly?"
                |> italic
                |> link "/about"
                |> el Zed [ inlineStyle styles ]
                |> el Zed [ paddingBottom (scaled 0) ]
        ]


messageScreen : String -> Node
messageScreen message =
    message
        |> String.split "\n"
        |> List.map italic
        |> column Message [ center ]
        |> el Zed [ center, verticalCenter ]
        |> el Zed [ height fill, paddingBottom (scaled 15) ]



-- Authentication


authButton : Authentication.Types.Method -> Node
authButton authMethod =
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
        ((++)
            [ center
            , height fill
            , maxWidth (px insulationWidth)
            , minHeight (px 218)
            , width fill
            ]
            (if insulationOptions.nested then
                [ clipX, yScrollbar ]
             else
                [ clip ]
            )
        )
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
            Alfred.entry context

        Nothing ->
            empty


backdropChosen : String -> Node
backdropChosen chosenBackdrop =
    let
        loadingImageDecoder =
            Settings.Types.SetLoadedBackdrop chosenBackdrop
                |> SettingsMsg
                |> Decode.succeed
    in
        Html.img
            [ Html.Attributes.style loadingImageStyles
            , Html.Attributes.src ("/images/Background/" ++ chosenBackdrop)
            , Html.Events.on "load" loadingImageDecoder
            ]
            []
            |> html


backdropLoaded : List String -> Bool -> Node
backdropLoaded loaded fadeInLastBackdrop =
    let
        amount =
            List.length loaded

        indexedMapFn idx item =
            el
                Zed
                [ item
                    |> backgroundImageStyles fadeInLastBackdrop (idx + 1 < amount)
                    |> inlineStyle
                ]
                empty
    in
        column Zed [] (List.indexedMap indexedMapFn loaded)


loadingImageStyles : List ( String, String )
loadingImageStyles =
    [ ( "height", "1px" )
    , ( "left", "100%" )
    , ( "opacity", "0.00001" )
    , ( "overflow", "hidden" )
    , ( "position", "fixed" )
    , ( "top", "100%" )
    , ( "transform", "translate(-1px, -1px)" )
    , ( "width", "1px" )
    , ( "z-index", "-10000" )
    ]


backgroundImageStyles : Bool -> Bool -> String -> List ( String, String )
backgroundImageStyles fadeInLastBackdrop isPrevious loadedBackdrop =
    let
        img =
            "url(/images/Background/" ++ loadedBackdrop ++ ")"
    in
        [ ( "animation"
          , if not isPrevious && fadeInLastBackdrop then
                "900ms linear 50ms forwards fadeIn"
            else
                "none"
          )
        , ( "background-image", img )
        , ( "background-size", "cover" )
        , ( "bottom", "-1px" )
        , ( "left", "-1px" )
        , ( "opacity"
          , if isPrevious || not fadeInLastBackdrop then
                "1"
            else
                "0"
          )
        , ( "position", "fixed" )
        , ( "right", "-1px" )
        , ( "top", "-1px" )
        , ( "z-index"
          , "-9"
          )
        , case loadedBackdrop of
            "1.jpg" ->
                ( "background-position", "center 30%" )

            "9.jpg" ->
                ( "background-position", "center 68%" )

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
