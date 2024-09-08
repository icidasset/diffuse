module Themes.Sunrise.Settings exposing (Dependencies, view)

import Chunky exposing (..)
import Color exposing (Color)
import Common exposing (ServiceWorkerStatus(..))
import Conditional exposing (ifThenElse)
import DateFormat as Date
import Html exposing (Html, text)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (onClick)
import Html.Lazy
import LastFm
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Themes.Sunrise.Kit as Kit
import Themes.Sunrise.Navigation as Navigation
import Themes.Sunrise.Settings.Data
import Themes.Sunrise.Settings.Sync
import Time
import UI.Backdrop as Backdrop exposing (backgroundPositioning)
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Settings.Page as Settings exposing (..)
import UI.Sources.Types as Sources
import UI.Tracks.Types as Tracks
import UI.Types exposing (Msg(..))
import User.Layer



-- 🗺


type alias Dependencies =
    { syncMethod : Maybe User.Layer.Method
    , buildTimestamp : Int
    , chosenBackgroundImage : Maybe String
    , coverSelectionReducesPool : Bool
    , currentTimeZone : Time.Zone
    , extractedBackdropColor : Maybe Color
    , hideDuplicateTracks : Bool
    , lastFm : LastFm.Model
    , processAutomatically : Bool
    , rememberProgress : Bool
    , serviceWorkerStatus : ServiceWorkerStatus
    , version : String
    }


view : Settings.Page -> Dependencies -> Html Msg
view page deps =
    case page of
        Data ->
            Themes.Sunrise.Settings.Data.view deps.syncMethod

        Index ->
            Kit.receptacle { scrolling = True } (index deps)

        Sync ->
            Themes.Sunrise.Settings.Sync.view deps.syncMethod



-- INDEX


index : Dependencies -> List (Html Msg)
index deps =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      Navigation.local
        [ ( Icon Icons.account_circle
          , Label "Data & Sync" Shown
          , NavigateToPage (Page.Settings Sync)
          )
        , ( Icon Icons.brush
          , Label "Change theme" Shown
          , PerformMsg AssistWithChangingTheme
          )
        , ( Icon Icons.help_outline
          , Label "Help" Shown
          , OpenLinkInNewPage "about/"
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , deps
        |> content
        |> chunk [ "pb-4" ]
        |> List.singleton
        |> Kit.canister
    ]


content : Dependencies -> List (Html Msg)
content deps =
    [ -----------------------------------------
      -- Title
      -----------------------------------------
      Kit.h1 "Settings"

    -----------------------------------------
    -- Version
    -----------------------------------------
    , chunk
        [ "mt-6" ]
        [ chunk
            [ "flex"
            , "flex-col"
            , "items-center"
            , "justify-center"
            , "text-base05"
            , "text-xs"

            -- Dark mode
            ------------
            , "dark:text-base03"
            ]
            [ slab
                Html.img
                [ A.src "images/diffuse-dark.svg"
                , A.width 160
                ]
                [ "dark:hidden" ]
                []

            --
            , slab
                Html.img
                [ A.src "images/diffuse-light.svg"
                , A.width 160
                ]
                [ "hidden dark:block" ]
                []

            --
            , chunk
                [ "italic", "mt-3", "text-center" ]
                [ text "Version "
                , text deps.version
                , lineBreak
                , text "Built on "
                , deps.buildTimestamp
                    |> (*) 1000
                    |> Time.millisToPosix
                    |> Date.format
                        [ Date.monthNameAbbreviated
                        , Date.text " "
                        , Date.dayOfMonthSuffix
                        , Date.text " "
                        , Date.yearNumber
                        , Date.text ", "
                        , Date.hourMilitaryFixed
                        , Date.text ":"
                        , Date.minuteFixed
                        , Date.text ":"
                        , Date.secondFixed
                        ]
                        deps.currentTimeZone
                    |> text

                --
                , chunk
                    [ "not-italic", "mt-3" ]
                    [ case deps.serviceWorkerStatus of
                        InstallingInitial ->
                            inline
                                [ "inline-flex", "items-center" ]
                                [ text "Setting up service worker"
                                , inline [ "ml-1" ] [ Icons.downloading 12 Inherit ]
                                ]

                        InstallingNew ->
                            inline
                                [ "inline-flex", "items-center" ]
                                [ text "Installing new version"
                                , inline [ "ml-1" ] [ Icons.downloading 12 Inherit ]
                                ]

                        WaitingForActivation ->
                            inline
                                []
                                [ text "Update available"
                                , brick
                                    [ Maybe.unwrap
                                        (class "bg-white-20")
                                        (style "background-color" << Color.toCssString)
                                        deps.extractedBackdropColor

                                    --
                                    , E.onClick ReloadApp
                                    ]
                                    [ "bg-base06"
                                    , "cursor-pointer"
                                    , "inline-block"
                                    , "leading-none"
                                    , "ml-1"
                                    , "p-1"
                                    , "rounded"
                                    , "text-white"

                                    -- Dark mode
                                    ------------
                                    , "dark:bg-base01"
                                    ]
                                    [ text "Reload app" ]
                                ]

                        Activated ->
                            nothing
                    ]
                ]
            ]
        ]

    -----------------------------------------
    -- Background
    -----------------------------------------
    , chunk
        [ "mt-6" ]
        [ label "Background Image"
        , Html.Lazy.lazy backgroundImage deps.chosenBackgroundImage
        ]

    -----------------------------------------
    -- Row 1
    -----------------------------------------
    , chunk
        [ "flex", "flex-wrap", "pt-2" ]
        [ chunk
            [ "w-full", "md:w-1/2" ]
            [ label "Downloaded tracks"
            , Kit.buttonWithColor
                Kit.Gray
                Kit.Normal
                (TracksMsg Tracks.ClearCache)
                (text "Clear cache")
            ]

        -- Last.fm
        ----------
        , chunk
            [ "w-1/2" ]
            [ label "Last.fm scrobbling"

            --
            , case ( deps.lastFm.authenticating, deps.lastFm.sessionKey ) of
                ( _, Just _ ) ->
                    Kit.checkbox
                        { checked = True
                        , toggleMsg = DisconnectLastFm
                        }

                ( True, Nothing ) ->
                    Kit.buttonWithColor
                        Kit.Gray
                        Kit.Normal
                        Bypass
                        (text "Connecting")

                ( False, Nothing ) ->
                    Kit.buttonWithColor
                        Kit.Gray
                        Kit.Normal
                        ConnectLastFm
                        (text "Connect")
            ]
        ]

    -----------------------------------------
    -- Row 2
    -----------------------------------------
    , chunk
        [ "flex", "flex-wrap" ]
        [ chunk
            [ "w-full", "md:w-1/2" ]
            [ label "Hide Duplicates"
            , Kit.checkbox
                { checked = deps.hideDuplicateTracks
                , toggleMsg = TracksMsg Tracks.ToggleHideDuplicates
                }
            ]
        , chunk
            [ "w-full", "md:w-1/2" ]
            [ label "Process sources automatically"
            , Kit.checkbox
                { checked = deps.processAutomatically
                , toggleMsg = SourcesMsg Sources.ToggleProcessAutomatically
                }
            ]
        ]

    -----------------------------------------
    -- Row 3
    -----------------------------------------
    , chunk
        [ "flex", "flex-wrap" ]
        [ chunk
            [ "w-full", "md:w-1/2" ]
            [ label "Remember position on long tracks"
            , Kit.checkbox
                { checked = deps.rememberProgress
                , toggleMsg = ToggleRememberProgress
                }
            ]
        , chunk
            [ "w-full", "md:w-1/2" ]
            [ label "Cover selection reduces track pool"
            , Kit.checkbox
                { checked = deps.coverSelectionReducesPool
                , toggleMsg = TracksMsg Tracks.ToggleCoverSelectionReducesPool
                }
            ]
        ]
    ]


label : String -> Html msg
label l =
    chunk
        [ "mb-3", "mt-6", "pb-px" ]
        [ Kit.label [] l ]



-- BACKGROUND IMAGE


backgroundImage : Maybe String -> Html Msg
backgroundImage chosenBackground =
    chunk
        [ "flex", "flex-wrap" ]
        (List.map
            (\( filename, _ ) ->
                let
                    isActive =
                        chosenBackground == Just filename
                in
                brick
                    [ onClick (ChooseBackdrop filename) ]
                    [ "cursor-pointer"
                    , "h-0"
                    , "overflow-hidden"
                    , "pt-1/8"
                    , "relative"
                    , "w-1/5"

                    --
                    , "md:pt-1/16"
                    , "md:w-1/10"
                    ]
                    [ if isActive then
                        chunk
                            [ "absolute"
                            , "bg-base04"
                            , "inset-0"
                            , "mb-1"
                            , "mr-1"
                            , "rounded-sm"
                            , "z-10"

                            --
                            , "sm:mb-2"
                            , "sm:mr-2"

                            --
                            , "md:mb-1"
                            , "md:mr-1"
                            ]
                            []

                      else
                        nothing

                    --
                    , brick
                        [ backgroundPositioning filename

                        --
                        , ")"
                            |> String.append filename
                            |> String.append "url(images/Background/Thumbnails/"
                            |> style "background-image"
                        ]
                        [ "absolute"
                        , "bg-cover"
                        , "inset-0"
                        , "mb-1"
                        , "mr-1"
                        , "rounded-sm"
                        , "z-20"

                        --
                        , "sm:mb-2"
                        , "sm:mr-2"

                        --
                        , "md:mb-1"
                        , "md:mr-1"

                        --
                        , ifThenElse isActive "opacity-20" "opacity-100"
                        ]
                        []

                    --
                    , if isActive then
                        chunk
                            [ "absolute"
                            , "inset-0"
                            , "flex"
                            , "font-semibold"
                            , "items-center"
                            , "justify-center"
                            , "leading-snug"
                            , "mb-1"
                            , "mr-1"
                            , "px-2"
                            , "text-center"
                            , "text-white"
                            , "text-xs"
                            , "z-30"

                            --
                            , "sm:mb-2"
                            , "sm:mr-2"

                            --
                            , "md:mb-1"
                            , "md:mr-1"

                            -- Dark mode
                            ------------
                            , "dark:text-base07"
                            ]
                            [ chunk
                                [ "mt-px" ]
                                [ Icons.check 16 Inherit ]
                            ]

                      else
                        nothing
                    ]
            )
            Backdrop.options
        )
