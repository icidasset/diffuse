module UI.View exposing (view)

import Alfred exposing (Alfred)
import Browser
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Html exposing (Html, section)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on)
import Html.Lazy as Lazy
import Json.Decode
import Maybe.Extra as Maybe
import UI.Alfred.View as Alfred
import UI.Authentication.Common as Authentication
import UI.Authentication.Types as Authentication
import UI.Authentication.View as Authentication
import UI.Backdrop as Backdrop
import UI.Console
import UI.ContextMenu
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page
import UI.Playlists.View as Playlists
import UI.Queue.View as Queue
import UI.Settings as Settings
import UI.Settings.Page
import UI.Sources.Page
import UI.Sources.View as Sources
import UI.Svg.Elements
import UI.Tracks.View as Tracks
import UI.Types exposing (..)
import User.Layer



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
        , Lazy.lazy2 UI.ContextMenu.view model.viewport.width model.contextMenu

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , Lazy.lazy2 UI.Notifications.view model.extractedBackdropColor model.notifications

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
                content
                    { opts | justifyCenter = True }
                    [ loadingAnimation
                    , chunk
                        [ "italic"
                        , "mt-5"
                        , "text-white"
                        , "text-opacity-30"
                        ]
                        [ case model.authentication of
                            Authentication.Authenticated _ ->
                                Html.text "Loading your data"

                            _ ->
                                Html.text "Transmitting particles"
                        ]
                    ]

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
                    (model.authentication
                        |> Authentication.extractMethod
                        |> Maybe.unwrap False User.Layer.methodSupportsPublicData
                    )

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
        [ "overflow-x-hidden"
        , "relative"
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
