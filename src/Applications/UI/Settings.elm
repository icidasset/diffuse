module UI.Settings exposing (Dependencies, view)

import Chunky exposing (..)
import Color.Ext as Color
import Conditional exposing (ifThenElse)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Settings
import UI.Backdrop as Backdrop exposing (backgroundPositioning)
import UI.Css
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Reply exposing (Reply(..))
import UI.Settings.ImportExport
import UI.Settings.Page as Settings exposing (..)
import User.Layer exposing (Method(..))



-- 🗺


type alias Dependencies =
    { authenticationMethod : Maybe User.Layer.Method
    , chosenBackgroundImage : Maybe String
    , hideDuplicateTracks : Bool
    , processAutomatically : Bool
    , rememberProgress : Bool
    }


view : Settings.Page -> Dependencies -> Html Reply
view page deps =
    case page of
        ImportExport ->
            UI.Settings.ImportExport.view deps.authenticationMethod

        Index ->
            UI.Kit.receptacle { scrolling = True } (index deps)



-- INDEX


index : Dependencies -> List (Html Reply)
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
          , PerformMsg SignOut
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , UI.Kit.canister
        [ UI.Kit.h1 "Settings"
        , [ text "Changes are saved automatically."
          , lineBreak
          , text "You're storing the data for this application "
          , case deps.authenticationMethod of
                Just Blockstack ->
                    text "on Blockstack."

                Just (Dropbox _) ->
                    text "on Dropbox."

                Just (Ipfs _) ->
                    text "on IPFS."

                Just Local ->
                    text "in this browser."

                Just (RemoteStorage _) ->
                    text "on a RemoteStorage server."

                Just (Textile _) ->
                    text "on Textile."

                Nothing ->
                    text "on nothing, wtf?"

          -- Change passphrase (if applicable)
          , case deps.authenticationMethod of
                Just Blockstack ->
                    nothing

                Just (Dropbox d) ->
                    changePassphrase (Dropbox d)

                Just (Ipfs i) ->
                    changePassphrase (Ipfs i)

                Just Local ->
                    changePassphrase Local

                Just (RemoteStorage r) ->
                    changePassphrase (RemoteStorage r)

                Just (Textile _) ->
                    nothing

                Nothing ->
                    nothing
          ]
            |> raw
            |> UI.Kit.intro

        -- Clear cache
        --------------
        , chunk
            [ C.flex, C.flex_wrap ]
            [ chunk
                [ C.w_full, C.md__w_half ]
                [ label "Downloaded tracks"
                , UI.Kit.buttonWithColor
                    UI.Kit.Gray
                    UI.Kit.Normal
                    ClearTracksCache
                    (text "Clear cache")
                ]
            , chunk
                [ C.w_full, C.md__w_half ]
                [ label "Hide Duplicates"
                , UI.Kit.checkbox
                    { checked = deps.hideDuplicateTracks
                    , toggleMsg = ToggleHideDuplicates
                    }
                ]
            ]

        -- Check it
        -----------
        , chunk
            [ C.flex, C.flex_wrap ]
            [ chunk
                [ C.w_full, C.md__w_half ]
                [ label "Process sources automatically"
                , UI.Kit.checkbox
                    { checked = deps.processAutomatically
                    , toggleMsg = ToggleProcessAutomatically
                    }
                ]
            , chunk
                [ C.w_full, C.md__w_half ]
                [ label "Remember position on long tracks"
                , UI.Kit.checkbox
                    { checked = deps.rememberProgress
                    , toggleMsg = ToggleRememberProgress
                    }
                ]
            ]

        -- Background image
        -------------------
        , label "Background Image"
        , Html.Lazy.lazy backgroundImage deps.chosenBackgroundImage
        ]
    ]


label : String -> Html msg
label l =
    chunk
        [ C.mb_3, C.mt_6, C.pb_px ]
        [ UI.Kit.label [] l ]



-- AUTHENTICATION


changePassphrase : User.Layer.Method -> Html Reply
changePassphrase method =
    inline
        []
        [ lineBreak
        , text "If you want to, you can "
        , UI.Kit.textButton
            { label = "change your passphrase"
            , onClick = ShowUpdateEncryptionKeyScreen method
            }
        , text "."
        ]



-- BACKGROUND IMAGE


backgroundImage : Maybe String -> Html Reply
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
                    , C.pt_1_div_5
                    , C.relative
                    , C.w_1_div_3

                    --
                    , C.md__pt_1_div_12
                    , C.md__w_1_div_7
                    ]
                    [ if isActive then
                        chunk
                            [ C.absolute
                            , C.bg_accent
                            , C.inset_0
                            , C.mb_1
                            , C.mr_1
                            , C.rounded_sm
                            , C.z_10
                            ]
                            []

                      else
                        chunk
                            [ C.absolute
                            , C.inset_0
                            , C.bg_black_05
                            , C.mb_1
                            , C.mr_1
                            , C.rounded_sm
                            , C.z_10
                            ]
                            []

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
                        , ifThenElse isActive C.opacity_20 C.opacity_100
                        ]
                        []

                    --
                    , if isActive then
                        chunk
                            [ C.absolute
                            , C.inset_0
                            , C.text_xs
                            , C.flex
                            , C.font_bold
                            , C.items_center
                            , C.justify_center
                            , C.leading_snug
                            , C.mb_1
                            , C.mr_1
                            , C.px_2
                            , C.text_center
                            , C.text_white
                            , C.z_30
                            ]
                            [ chunk
                                [ C.hidden
                                , C.md__visible
                                ]
                                [ text "Selected" ]
                            ]

                      else
                        nothing
                    ]
            )
            Backdrop.options
        )
