module UI.View exposing (view)

import Alfred exposing (Alfred)
import Browser
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Css.Classes as C
import Html exposing (Html, section)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on, onClick)
import Html.Events.Extra.Pointer as Pointer
import Html.Lazy as Lazy
import Json.Decode
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Playlists.Encoding as Playlists
import Queue
import Settings
import Sources
import Sources.Encoding as Sources
import Tracks
import Tracks.Encoding as Tracks
import UI.Alfred.View as Alfred
import UI.Authentication.Common as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Authentication.Types as Authentication
import UI.Authentication.View as Authentication
import UI.Backdrop as Backdrop
import UI.Console
import UI.ContextMenu
import UI.Equalizer.View as Equalizer
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.View as Playlists
import UI.Queue.View as Queue
import UI.Settings as Settings
import UI.Settings.Page
import UI.Sources.ContextMenu as Sources
import UI.Sources.Page
import UI.Sources.View as Sources
import UI.Svg.Elements
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.View as Tracks
import UI.Types exposing (..)
import Url exposing (Protocol(..))
import User.Layer exposing (..)



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    section
        (if Maybe.isJust model.contextMenu || Maybe.isJust model.alfred then
            [ on "tap" (Json.Decode.succeed HideOverlay) ]

         else if Maybe.isJust model.eqKnobOperation then
            [ Pointer.onMove AdjustKnob
            , Pointer.onUp DeactivateKnob
            , Pointer.onCancel DeactivateKnob
            ]

         else if model.isDragging then
            [ class C.dragging_something
            , on "mouseup" (Json.Decode.succeed StoppedDragging)
            , on "touchcancel" (Json.Decode.succeed StoppedDragging)
            , on "touchend" (Json.Decode.succeed StoppedDragging)
            ]

         else if Maybe.isJust model.selectedQueueItem then
            [ on "tap" (Json.Decode.succeed RemoveQueueSelection) ]

         else if not (List.isEmpty model.selectedTrackIndexes) then
            [ on "tap" (Json.Decode.succeed RemoveTrackSelection) ]

         else
            []
        )
        [ -----------------------------------------
          -- Alfred
          -----------------------------------------
          Lazy.lazy Alfred.view model.alfred

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , Lazy.lazy Backdrop.view model

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        , Lazy.lazy UI.ContextMenu.view model.contextMenu

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , Lazy.lazy UI.Notifications.view model.notifications

        -----------------------------------------
        -- Overlay
        -----------------------------------------
        , Lazy.lazy2 overlay model.alfred model.contextMenu

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
                content opts [ Authentication.view model ]
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
        model.alfred
        model.page

    -----------------------------------------
    -- Main
    -----------------------------------------
    , vessel
        [ Tracks.view
            model
            { amountOfSources = List.length model.sources
            , bgColor = model.extractedBackdropColor
            , darkMode = model.darkMode
            , isOnIndexPage = model.page == Page.Index
            , isTouchDevice = model.isTouchDevice
            , sourceIdsBeingProcessed = List.map Tuple.first model.processingContext
            , viewport = model.viewport
            }

        -- Pages
        --------
        , case model.page of
            Page.Equalizer ->
                Lazy.lazy Equalizer.view model.eqSettings

            Page.Index ->
                nothing

            Page.Playlists subPage ->
                Lazy.lazy5
                    Playlists.view
                    subPage
                    model.playlists
                    model.selectedPlaylist
                    model.editPlaylistContext
                    model.extractedBackdropColor

            Page.Queue subPage ->
                Queue.view subPage model

            Page.Settings subPage ->
                Lazy.lazy2 Settings.view
                    subPage
                    { authenticationMethod = Authentication.extractMethod model.authentication
                    , chosenBackgroundImage = model.chosenBackdrop
                    , hideDuplicateTracks = model.hideDuplicates
                    , lastFm = model.lastFm
                    , processAutomatically = model.processAutomatically
                    , rememberProgress = model.rememberProgress
                    }

            Page.Sources subPage ->
                Sources.view subPage model
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , UI.Console.view
        model.nowPlaying
        model.repeat
        model.shuffle
        { stalled = model.audioHasStalled
        , loading = model.audioIsLoading
        , playing = model.audioIsPlaying
        }
        ( model.audioPosition
        , model.audioDuration
        )
    ]



-- 🗺  ░░  BITS


content : { justifyCenter : Bool, scrolling : Bool } -> List (Html Msg) -> Html Msg
content { justifyCenter, scrolling } nodes =
    brick
        [ on "focusout" (Json.Decode.succeed Blur)
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
                        Json.Decode.succeed FocusedOnInput

                    "TEXTAREA" ->
                        Json.Decode.succeed FocusedOnInput

                    _ ->
                        Json.Decode.fail "NOT_INPUT"
            )


loadingAnimation : Html msg
loadingAnimation =
    Html.map never UI.Svg.Elements.loading


overlay : Maybe (Alfred Msg) -> Maybe (ContextMenu Msg) -> Html Msg
overlay maybeAlfred maybeContextMenu =
    let
        isShown =
            Maybe.isJust maybeAlfred || Maybe.isJust maybeContextMenu
    in
    brick
        [ onClick HideOverlay ]
        [ C.inset_0
        , C.bg_black
        , C.duration_1000
        , C.ease_in_out
        , C.fixed
        , C.transition
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
