module UI.View exposing (view)

import Alfred exposing (Alfred)
import Alien
import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Css exposing (url)
import Css.Classes as C
import Debouncer.Basic as Debouncer
import Dict
import Dict.Ext as Dict
import File
import File.Download
import File.Select
import Html exposing (Html, section)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)
import Html.Events.Extra.Pointer as Pointer
import Html.Lazy as Lazy
import Json.Decode
import Json.Encode
import LastFm
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Lens as Lens
import Notifications
import Playlists.Encoding as Playlists
import Process
import Queue
import Return2 exposing (..)
import Return3
import Settings
import Sources
import Sources.Encoding as Sources
import Sources.Services.Dropbox
import Sources.Services.Google
import String.Ext as String
import Task
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Alfred as Alfred
import UI.Audio.State as Audio
import UI.Audio.Types as Audio
import UI.Authentication as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Backdrop as Backdrop
import UI.Common.State exposing (showNotification, showNotificationWithModel)
import UI.Console
import UI.ContextMenu
import UI.Demo as Demo
import UI.Equalizer as Equalizer
import UI.Interface.State as Interface
import UI.Interface.Types as Interface
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page
import UI.Playlists as Playlists
import UI.Playlists.Alfred
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.Directory
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.ContextMenu as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Routing.State as Routing
import UI.Settings as Settings
import UI.Settings.Page
import UI.Sources as Sources
import UI.Sources.ContextMenu as Sources
import UI.Sources.Form
import UI.Sources.Page
import UI.Svg.Elements
import UI.Tracks as Tracks
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.Scene.List
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (..)
import UI.User.State as User
import Url exposing (Protocol(..), Url)
import Url.Ext as Url
import User.Layer exposing (..)
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    section
        (if Maybe.isJust model.contextMenu || Maybe.isJust model.alfred.instance then
            [ on "tap" (interfaceEventHandler Interface.HideOverlay) ]

         else if Maybe.isJust model.equalizer.activeKnob then
            [ Pointer.onMove (EqualizerMsg << Equalizer.AdjustKnob)
            , Pointer.onUp (EqualizerMsg << Equalizer.DeactivateKnob)
            , Pointer.onCancel (EqualizerMsg << Equalizer.DeactivateKnob)
            ]

         else if model.isDragging then
            [ class C.dragging_something
            , on "mouseup" (interfaceEventHandler Interface.StoppedDragging)
            , on "touchcancel" (interfaceEventHandler Interface.StoppedDragging)
            , on "touchend" (interfaceEventHandler Interface.StoppedDragging)
            ]

         else if Maybe.isJust model.queue.selection then
            [ on "tap" (interfaceEventHandler Interface.RemoveQueueSelection) ]

         else if not (List.isEmpty model.tracks.selectedTrackIndexes) then
            [ on "tap" (interfaceEventHandler Interface.RemoveTrackSelection) ]

         else
            []
        )
        [ -----------------------------------------
          -- Alfred
          -----------------------------------------
          model.alfred
            |> Lazy.lazy Alfred.view
            |> Html.map AlfredMsg

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , model.backdrop
            |> Lazy.lazy Backdrop.view
            |> Html.map BackdropMsg

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        , model.contextMenu
            |> Lazy.lazy UI.ContextMenu.view
            |> Html.map Reply

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , model.notifications
            |> Lazy.lazy UI.Notifications.view
            |> Html.map Reply

        -----------------------------------------
        -- Overlay
        -----------------------------------------
        , model.contextMenu
            |> Lazy.lazy2 overlay model.alfred.instance

        -----------------------------------------
        -- Content
        -----------------------------------------
        , let
            opts =
                { justifyCenter = False
                , scrolling = not model.isDragging
                }
          in
          case ( model.isLoading, model.authentication ) of
            ( True, _ ) ->
                content { opts | justifyCenter = True } [ loadingAnimation ]

            ( False, Authentication.Authenticated _ ) ->
                content opts (defaultScreen model)

            ( False, _ ) ->
                model.authentication
                    |> Lazy.lazy Authentication.view
                    |> Html.map AuthenticationMsg
                    |> List.singleton
                    |> content opts
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ Lazy.lazy2
        (Navigation.global
            [ ( Page.Index, "Tracks" )
            , ( Page.Sources UI.Sources.Page.Index, "Sources" )
            , ( Page.Settings UI.Settings.Page.Index, "Settings" )
            ]
        )
        model.alfred.instance
        model.page

    -----------------------------------------
    -- Main
    -----------------------------------------
    , vessel
        [ { amountOfSources = List.length model.sources.collection
          , bgColor = model.backdrop.bgColor
          , darkMode = model.darkMode
          , isOnIndexPage = model.page == Page.Index
          , isTouchDevice = model.isTouchDevice
          , sourceIdsBeingProcessed = List.map Tuple.first model.sources.isProcessing
          , viewport = model.viewport
          }
            |> Tracks.view model.tracks
            |> Html.map TracksMsg

        -- Pages
        --------
        , case model.page of
            Page.Equalizer ->
                model.equalizer
                    |> Lazy.lazy Equalizer.view
                    |> Html.map EqualizerMsg

            Page.Index ->
                nothing

            Page.Playlists subPage ->
                model.backdrop.bgColor
                    |> Lazy.lazy4
                        Playlists.view
                        subPage
                        model.playlists
                        model.tracks.selectedPlaylist
                    |> Html.map PlaylistsMsg

            Page.Queue subPage ->
                model.queue
                    |> Lazy.lazy2 Queue.view subPage
                    |> Html.map QueueMsg

            Page.Settings subPage ->
                { authenticationMethod = Authentication.extractMethod model.authentication
                , chosenBackgroundImage = model.backdrop.chosen
                , hideDuplicateTracks = model.tracks.hideDuplicates
                , lastFm = model.lastFm
                , processAutomatically = model.processAutomatically
                , rememberProgress = model.audio.rememberProgress
                }
                    |> Lazy.lazy2 Settings.view subPage
                    |> Html.map Reply

            Page.Sources subPage ->
                let
                    amountOfTracks =
                        List.length model.tracks.collection.untouched
                in
                model.sources
                    |> Lazy.lazy3 Sources.view { amountOfTracks = amountOfTracks } subPage
                    |> Html.map SourcesMsg
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , Html.map Reply
        (UI.Console.view
            model.queue.activeItem
            model.queue.repeat
            model.queue.shuffle
            { stalled = model.audio.hasStalled
            , loading = model.audio.isLoading
            , playing = model.audio.isPlaying
            }
            ( model.audio.position
            , model.audio.duration
            )
        )
    ]



-- 🗺  ░░  BITS


content : { justifyCenter : Bool, scrolling : Bool } -> List (Html Msg) -> Html Msg
content { justifyCenter, scrolling } nodes =
    brick
        [ on "focusout" (interfaceEventHandler Interface.Blur)
        , on "focusin" inputFocusDecoder
        , style "height" "calc(var(--vh, 1vh) * 100)"
        ]
        [ C.overflow_x_hidden
        , C.relative
        , C.scrolling_touch
        , C.w_screen
        , C.z_10

        --
        , ifThenElse scrolling C.overflow_y_auto C.overflow_y_hidden
        ]
        [ brick
            [ style "min-width" "280px" ]
            [ C.flex
            , C.flex_col
            , C.items_center
            , C.h_full
            , C.px_4

            --
            , C.md__px_8
            , C.lg__px_16

            --
            , ifThenElse justifyCenter C.justify_center ""
            ]
            nodes
        ]


inputFocusDecoder : Json.Decode.Decoder Msg
inputFocusDecoder =
    Json.Decode.string
        |> Json.Decode.at [ "target", "tagName" ]
        |> Json.Decode.andThen
            (\targetTagName ->
                case targetTagName of
                    "INPUT" ->
                        interfaceEventHandler Interface.FocusedOnInput

                    "TEXTAREA" ->
                        interfaceEventHandler Interface.FocusedOnInput

                    _ ->
                        Json.Decode.fail "NOT_INPUT"
            )


interfaceEventHandler : Interface.Msg -> Json.Decode.Decoder UI.Msg
interfaceEventHandler =
    Interface >> Json.Decode.succeed


loadingAnimation : Html msg
loadingAnimation =
    Html.map never UI.Svg.Elements.loading


overlay : Maybe (Alfred Reply) -> Maybe (ContextMenu Reply) -> Html Msg
overlay maybeAlfred maybeContextMenu =
    let
        isShown =
            Maybe.isJust maybeAlfred || Maybe.isJust maybeContextMenu
    in
    brick
        [ onClick (Interface Interface.HideOverlay) ]
        [ C.inset_0
        , C.bg_black
        , C.fixed
        , C.transition_1000
        , C.transition_ease
        , C.transition_opacity
        , C.z_30

        --
        , ifThenElse isShown "" C.pointer_events_none
        , ifThenElse isShown C.opacity_40 C.opacity_0
        ]
        []


vessel : List (Html Msg) -> Html Msg
vessel =
    (>>)
        (brick
            [ style "-webkit-mask-image" "-webkit-radial-gradient(white, black)" ]
            [ C.bg_white
            , C.flex
            , C.flex_col
            , C.flex_grow
            , C.overflow_hidden
            , C.relative
            , C.rounded

            -- Dark mode
            ------------
            , C.dark__bg_darkest_hour
            ]
        )
        (bricky
            [ style "min-height" "296px" ]
            [ C.flex
            , C.flex_grow
            , C.rounded
            , C.shadow_lg
            , C.w_full

            --
            , C.lg__max_w_insulation
            , C.lg__min_w_3xl
            ]
        )
