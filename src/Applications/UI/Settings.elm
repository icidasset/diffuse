module UI.Settings exposing (Dependencies, view)

import Chunky exposing (..)
import Conditional exposing (ifThenElse)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy
import LastFm
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Settings
import UI.Authentication.Types as Authentication
import UI.Backdrop as Backdrop exposing (backgroundPositioning)
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Settings.ImportExport
import UI.Settings.Page as Settings exposing (..)
import UI.Sources.Types as Sources
import UI.Tracks.Types as Tracks
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..))



-- 🗺


type alias Dependencies =
    { authenticationMethod : Maybe User.Layer.Method
    , chosenBackgroundImage : Maybe String
    , hideDuplicateTracks : Bool
    , lastFm : LastFm.Model
    , processAutomatically : Bool
    , rememberProgress : Bool
    }


view : Settings.Page -> Dependencies -> Html Msg
view page deps =
    case page of
        ImportExport ->
            UI.Settings.ImportExport.view deps.authenticationMethod

        Index ->
            UI.Kit.receptacle { scrolling = True } (index deps)



-- INDEX


index : Dependencies -> List (Html Msg)
index deps =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.import_export
          , Label "Import & Export" Shown
          , NavigateToPage (Page.Settings ImportExport)
          )
        , ( Icon Icons.help_outline
          , Label "Help" Shown
          , OpenLinkInNewPage "about#UI"
          )
        , ( Icon Icons.exit_to_app
          , Label "Sign out" Shown
          , PerformMsg (AuthenticationMsg Authentication.SignOut)
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , deps
        |> content
        |> chunk [ C.pb_4 ]
        |> List.singleton
        |> UI.Kit.canister
    ]


content : Dependencies -> List (Html Msg)
content deps =
    [ -----------------------------------------
      -- Title
      -----------------------------------------
      UI.Kit.h1 "Settings"

    -----------------------------------------
    -- Intro
    -----------------------------------------
    , [ text "Changes are saved automatically."
      , lineBreak
      , text "You're storing the data for this application "
      , case deps.authenticationMethod of
            Just (Dropbox _) ->
                text "on Dropbox."

            Just Fission ->
                text "on Fission."

            Just (Ipfs _) ->
                text "on IPFS."

            Just Local ->
                text "in this browser."

            Just (RemoteStorage _) ->
                text "on a RemoteStorage server."

            Nothing ->
                text "on nothing, wtf?"

      -- Change passphrase (if applicable)
      , case deps.authenticationMethod of
            Just (Dropbox d) ->
                changePassphrase (Dropbox d)

            Just Fission ->
                nothing

            Just (Ipfs i) ->
                changePassphrase (Ipfs i)

            Just Local ->
                changePassphrase Local

            Just (RemoteStorage r) ->
                changePassphrase (RemoteStorage r)

            Nothing ->
                nothing
      ]
        |> raw
        |> UI.Kit.intro

    -----------------------------------------
    -- Background
    -----------------------------------------
    , chunk
        [ C.mt_8 ]
        [ label "Background Image"
        , Html.Lazy.lazy backgroundImage deps.chosenBackgroundImage
        ]

    -----------------------------------------
    -- Row 1
    -----------------------------------------
    , chunk
        [ C.flex, C.flex_wrap, C.pt_2 ]
        [ chunk
            [ C.w_full, C.md__w_1over2 ]
            [ label "Downloaded tracks"
            , UI.Kit.buttonWithColor
                UI.Kit.Gray
                UI.Kit.Normal
                (TracksMsg Tracks.ClearCache)
                (text "Clear cache")
            ]

        -- Last.fm
        ----------
        , chunk
            [ C.w_1over2 ]
            [ label "Last.fm scrobbling"

            --
            , case ( deps.lastFm.authenticating, deps.lastFm.sessionKey ) of
                ( _, Just _ ) ->
                    UI.Kit.checkbox
                        { checked = True
                        , toggleMsg = DisconnectLastFm
                        }

                ( True, Nothing ) ->
                    UI.Kit.buttonWithColor
                        UI.Kit.Gray
                        UI.Kit.Normal
                        Bypass
                        (text "Connecting")

                ( False, Nothing ) ->
                    UI.Kit.buttonWithColor
                        UI.Kit.Gray
                        UI.Kit.Normal
                        ConnectLastFm
                        (text "Connect")
            ]
        ]

    -----------------------------------------
    -- Row 2
    -----------------------------------------
    , chunk
        [ C.flex, C.flex_wrap ]
        [ chunk
            [ C.w_full, C.md__w_1over2 ]
            [ label "Hide Duplicates"
            , UI.Kit.checkbox
                { checked = deps.hideDuplicateTracks
                , toggleMsg = TracksMsg Tracks.ToggleHideDuplicates
                }
            ]
        , chunk
            [ C.w_full, C.md__w_1over2 ]
            [ label "Process sources automatically"
            , UI.Kit.checkbox
                { checked = deps.processAutomatically
                , toggleMsg = SourcesMsg Sources.ToggleProcessAutomatically
                }
            ]
        ]

    -----------------------------------------
    -- Row 3
    -----------------------------------------
    , chunk
        [ C.flex, C.flex_wrap ]
        [ chunk
            [ C.w_full, C.md__w_1over2 ]
            [ label "Remember position on long tracks"
            , UI.Kit.checkbox
                { checked = deps.rememberProgress
                , toggleMsg = ToggleRememberProgress
                }
            ]
        ]
    ]


label : String -> Html msg
label l =
    chunk
        [ C.mb_3, C.mt_6, C.pb_px ]
        [ UI.Kit.label [] l ]



-- AUTHENTICATION


changePassphrase : User.Layer.Method -> Html Msg
changePassphrase method =
    inline
        []
        [ lineBreak
        , text "If you want to, you can "
        , UI.Kit.textButton
            { label = "change your passphrase"
            , onClick =
                method
                    |> Authentication.ShowUpdateEncryptionKeyScreen
                    |> AuthenticationMsg
            }
        , text "."
        ]



-- BACKGROUND IMAGE


backgroundImage : Maybe String -> Html Msg
backgroundImage chosenBackground =
    chunk
        [ C.flex, C.flex_wrap ]
        (List.map
            (\( filename, backdropLabel ) ->
                let
                    isActive =
                        chosenBackground == Just filename
                in
                brick
                    [ onClick (ChooseBackdrop filename) ]
                    [ C.cursor_pointer
                    , C.h_0
                    , C.overflow_hidden
                    , C.pt_1over8
                    , C.relative
                    , C.w_1over5

                    --
                    , C.md__pt_1over16
                    , C.md__w_1over10
                    ]
                    [ if isActive then
                        chunk
                            [ C.absolute
                            , C.bg_base04
                            , C.inset_0
                            , C.mb_1
                            , C.mr_1
                            , C.rounded_sm
                            , C.z_10

                            --
                            , C.sm__mb_2
                            , C.sm__mr_2

                            --
                            , C.md__mb_1
                            , C.md__mr_1
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
                        [ C.absolute
                        , C.bg_cover
                        , C.inset_0
                        , C.mb_1
                        , C.mr_1
                        , C.rounded_sm
                        , C.z_20

                        --
                        , C.sm__mb_2
                        , C.sm__mr_2

                        --
                        , C.md__mb_1
                        , C.md__mr_1

                        --
                        , ifThenElse isActive C.opacity_20 C.opacity_100
                        ]
                        []

                    --
                    , if isActive then
                        chunk
                            [ C.absolute
                            , C.inset_0
                            , C.flex
                            , C.font_semibold
                            , C.items_center
                            , C.justify_center
                            , C.leading_snug
                            , C.mb_1
                            , C.mr_1
                            , C.px_2
                            , C.text_center
                            , C.text_white
                            , C.text_xs
                            , C.z_30

                            --
                            , C.sm__mb_2
                            , C.sm__mr_2

                            --
                            , C.md__mb_1
                            , C.md__mr_1

                            -- Dark mode
                            ------------
                            , C.dark__text_base07
                            ]
                            [ chunk
                                [ C.mt_px ]
                                [ Icons.check 16 Inherit ]
                            ]

                      else
                        nothing
                    ]
            )
            Backdrop.options
        )
