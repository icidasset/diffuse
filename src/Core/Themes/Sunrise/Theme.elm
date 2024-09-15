module Themes.Sunrise.Theme exposing (theme)

import Alfred exposing (Alfred)
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Html exposing (Html, section)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on)
import Html.Lazy as Lazy
import Json.Decode
import Material.Icons as Icons
import Maybe.Extra as Maybe
import Theme exposing (Theme)
import Themes.Sunrise.Alfred.View as Alfred
import Themes.Sunrise.Console
import Themes.Sunrise.ContextMenu as ContextMenu
import Themes.Sunrise.Navigation as Navigation
import Themes.Sunrise.Notifications as Notifications
import Themes.Sunrise.Playlists.View as Playlists
import Themes.Sunrise.Queue.View as Queue
import Themes.Sunrise.Settings as Settings
import Themes.Sunrise.Sources.View as Sources
import Themes.Sunrise.Syncing.View as Syncing
import Themes.Sunrise.Tracks.View as Tracks
import UI.Backdrop as Backdrop
import UI.Page as Page
import UI.Settings.Page
import UI.Sources.Page
import UI.Syncing.Common as Syncing
import UI.Types exposing (..)
import User.Layer


theme : Theme Msg Model
theme =
    { id = "sunrise"
    , title = "Sunrise"
    , icon = Icons.wb_sunny
    , view = view
    }



-- ㊙️


view : Model -> Html Msg
view model =
    section
        (if Maybe.isJust model.contextMenu || Maybe.isJust model.alfred then
            [ on "tap" (Json.Decode.succeed HideOverlay) ]

         else if model.showVolumeSlider then
            [ on "tap" (Json.Decode.succeed <| ToggleVolumeSlider Off) ]

         else if model.isDragging then
            [ class "dragging-something"
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
          Lazy.lazy2 Alfred.view model.alfred model.extractedBackdropColor

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , Lazy.lazy Backdrop.view model

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        , Lazy.lazy2 ContextMenu.view model.viewport.width model.contextMenu

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , Lazy.lazy2 Notifications.view model.extractedBackdropColor model.notifications

        -----------------------------------------
        -- Overlay
        -----------------------------------------
        , Lazy.lazy2 overlay model.alfred model.contextMenu

        -----------------------------------------
        -- Content
        -----------------------------------------
        , content
            { justifyCenter = False
            , scrolling = not model.isDragging
            }
            (defaultScreen model)
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
        [ Tracks.view model

        -- Pages
        --------
        , case model.page of
            Page.Index ->
                nothing

            Page.Playlists subPage ->
                Lazy.lazy6
                    Playlists.view
                    subPage
                    model.playlists
                    model.selectedPlaylist
                    model.editPlaylistContext
                    model.extractedBackdropColor
                    (model.syncing
                        |> Syncing.extractMethod
                        |> Maybe.unwrap False User.Layer.methodSupportsPublicData
                    )

            Page.Queue subPage ->
                Queue.view subPage model

            Page.Settings subPage ->
                Lazy.lazy2 Settings.view
                    subPage
                    { buildTimestamp = model.buildTimestamp
                    , chosenBackgroundImage = model.chosenBackdrop
                    , coverSelectionReducesPool = model.coverSelectionReducesPool
                    , currentTimeZone = model.currentTimeZone
                    , extractedBackdropColor = model.extractedBackdropColor
                    , hideDuplicateTracks = model.hideDuplicates
                    , lastFm = model.lastFm
                    , processAutomatically = model.processAutomatically
                    , rememberProgress = model.rememberProgress
                    , serviceWorkerStatus = model.serviceWorkerStatus
                    , syncMethod = Syncing.extractMethod model.syncing
                    , version = model.version
                    }

            Page.Sources subPage ->
                Sources.view subPage model

        -- Syncing
        ----------
        , Syncing.view model
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , Themes.Sunrise.Console.view
        model.nowPlaying
        model.repeat
        model.shuffle
    ]



-- 🗺  ░░  BITS


content : { justifyCenter : Bool, scrolling : Bool } -> List (Html Msg) -> Html Msg
content { justifyCenter, scrolling } nodes =
    brick
        [ on "focusout" (Json.Decode.succeed Blur)
        , on "focusin" inputFocusDecoder
        ]
        [ "overflow-x-hidden"
        , "relative"
        , "screen-height"
        , "scrolling-touch"
        , "w-screen"
        , "z-10"

        --
        , ifThenElse scrolling "overflow-y-auto" "overflow-y-hidden"
        ]
        [ brick
            [ style "min-width" "280px" ]
            [ "flex"
            , "flex-col"
            , "items-center"
            , "h-full"
            , "px-3"

            --
            , "md:px-8"
            , "lg:px-16"

            --
            , ifThenElse justifyCenter "justify-center" "justify-start"
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


overlay : Maybe (Alfred Msg) -> Maybe (ContextMenu Msg) -> Html Msg
overlay maybeAlfred maybeContextMenu =
    let
        isShown =
            Maybe.isJust maybeAlfred || Maybe.isJust maybeContextMenu
    in
    brick
        []
        [ "inset-0"
        , "bg-darkest-hour"
        , "duration-500"
        , "ease-in-out"
        , "fixed"
        , "transition-opacity"
        , "z-30"

        --
        , ifThenElse isShown "pointer-events-auto" "pointer-events-none"
        , ifThenElse isShown "opacity-50" "opacity-0"
        ]
        []


vessel : List (Html Msg) -> Html Msg
vessel =
    brick
        [ style "-webkit-mask-image" "-webkit-radial-gradient(white, black)" ]
        [ "bg-white"
        , "flex"
        , "flex-col"
        , "flex-grow"
        , "overflow-hidden"
        , "relative"
        , "rounded"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        ]
        >> bricky
            [ style "min-height" "296px" ]
            [ "flex"
            , "flex-grow"
            , "rounded"
            , "shadow-lg"
            , "w-full"

            --
            , "lg:max-w-insulation"
            , "lg:min-w-3xl"
            ]
