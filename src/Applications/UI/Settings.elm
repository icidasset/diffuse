module UI.Settings exposing (view)

import Authentication exposing (Method(..))
import Chunky exposing (..)
import Color.Ext as Color
import Conditional exposing (ifThenElse)
import Css
import Css.Media
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css, selected, value)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Tachyons.Classes as T
import UI.Authentication as Authentication
import UI.Backdrop as Backdrop
import UI.Core as Core
import UI.Css
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Settings.ImportExport
import UI.Settings.Page as Settings exposing (..)
import UI.Tracks as Tracks



-- 🗺


view : Settings.Page -> Core.Model -> Html Core.Msg
view page model =
    case page of
        ImportExport ->
            UI.Settings.ImportExport.view

        Index ->
            UI.Kit.receptacle { scrolling = True } (index model)



-- INDEX


index : Core.Model -> List (Html Core.Msg)
index model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.import_export
          , Label "Import & Export" Shown
          , NavigateToPage (Page.Settings ImportExport)
          )
        , ( Icon Icons.exit_to_app
          , Label "Sign out" Shown
          , PerformMsg Core.SignOut
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , UI.Kit.canister
        [ UI.Kit.h1 "Settings"
        , [ text "Changes are saved automatically."
          , lineBreak
          , text "PS. You're storing the data for this application "
          , case Authentication.extractMethod model.authentication of
                Just Blockstack ->
                    text "on Blockstack."

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
          , case Authentication.extractMethod model.authentication of
                Just Blockstack ->
                    nothing

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

        -- Duplicates
        -------------
        , chunk [ T.mb3, T.mt4 ] [ UI.Kit.label [] "Hide Duplicates" ]
        , UI.Kit.checkbox
            { checked = model.tracks.hideDuplicates
            , toggleMsg = Core.TracksMsg Tracks.ToggleHideDuplicates
            }

        -- Background image
        -------------------
        , chunk [ T.mb3, T.mt4 ] [ UI.Kit.label [] "Background Image" ]
        , Html.Styled.Lazy.lazy backgroundImage model.backdrop.chosen
        ]
    ]



-- AUTHENTICATION


changePassphrase : Authentication.Method -> Html Core.Msg
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
                    |> Core.AuthenticationMsg
            }
        , text "."
        ]



-- BACKGROUND IMAGE


backgroundImage : Maybe String -> Html Core.Msg
backgroundImage chosenBackground =
    chunk
        [ T.flex, T.flex_wrap ]
        (List.map
            (\( filename, backdropLabel ) ->
                let
                    isActive =
                        chosenBackground == Just filename
                in
                brick
                    [ css backgroundThumbnailStyles
                    , onClick (Core.BackdropMsg <| Backdrop.Choose filename)
                    ]
                    [ T.overflow_hidden
                    , T.pointer
                    , T.relative
                    ]
                    [ if isActive then
                        brick
                            [ css backgroundThumbnailColorStyles ]
                            [ T.absolute
                            , T.absolute__fill
                            , T.br1
                            , T.mb1
                            , T.mr1
                            , T.z_1
                            ]
                            []

                      else
                        chunk
                            [ T.absolute
                            , T.absolute__fill
                            , T.bg_black_05
                            , T.br1
                            , T.mb1
                            , T.mr1
                            , T.z_1
                            ]
                            []

                    --
                    , brick
                        [ css (backgroundThumbnailInnerStyles filename)
                        , Backdrop.backgroundPositioning filename
                        ]
                        [ T.absolute
                        , T.absolute__fill
                        , T.br1
                        , T.mb1
                        , T.mr1
                        , T.z_2
                        , ifThenElse isActive T.o_20 T.o_100
                        ]
                        []

                    --
                    , if isActive then
                        chunk
                            [ T.absolute
                            , T.absolute__fill
                            , T.f7
                            , T.flex
                            , T.fw7
                            , T.items_center
                            , T.justify_center
                            , T.lh_title
                            , T.mb1
                            , T.mr1
                            , T.ph2
                            , T.tc
                            , T.white
                            , T.z_3
                            ]
                            [ chunk
                                [ T.dn
                                , T.db_ns
                                ]
                                [ text "Selected" ]
                            ]

                      else
                        nothing
                    ]
            )
            Backdrop.options
        )


backgroundThumbnailStyles : List Css.Style
backgroundThumbnailStyles =
    [ Css.height Css.zero
    , Css.paddingTop (Css.pct 19.3083198175)
    , Css.width (Css.pct 33.33333)

    --
    , Css.Media.withMedia
        [ UI.Css.notSmallMediaQuery ]
        [ Css.paddingTop (Css.pct 8.275)
        , Css.width (Css.pct 14.28571)
        ]
    ]


backgroundThumbnailColorStyles : List Css.Style
backgroundThumbnailColorStyles =
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colorKit.accent) ]


backgroundThumbnailInnerStyles : String -> List Css.Style
backgroundThumbnailInnerStyles filename =
    [ Css.backgroundImage (Css.url <| "images/Background/Thumbnails/" ++ filename)
    , Css.backgroundSize Css.cover
    ]
