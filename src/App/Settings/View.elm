module Settings.View exposing (..)

import Authentication.Types as Authentication exposing (Method(..))
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types exposing (Page(Abroad))
import Settings.State exposing (defaultBackdrop)
import Settings.Types exposing (Msg(..))
import Types as TopLevel exposing (Model, Msg(..))
import Variables exposing (scaled)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Ext
import Element.Input as Input
import Element.Types exposing (Node)
import Layouts


-- Styles

import Form.Styles
import Styles exposing (Styles(..))


-- 🍯


entry : Model -> Node
entry model =
    column
        Zed
        []
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            [ ( Icon Icons.import_export
              , Label (Shown "Import / Export")
                --
              , RoutingMsg (Routing.Types.GoToPage Abroad)
              )
            , ( Icon Icons.exit_to_app
              , Label (Shown "Sign out")
                --
              , AuthenticationMsg Authentication.PerformSignOut
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , column
            Zed
            [ paddingXY (scaled 4) 0 ]
            [ Layouts.h1 "Settings"
            , Layouts.intro
                [ text "Changes are automatically saved."
                , Element.Ext.lineBreak
                , text "PS. You are using the "
                , case Maybe.withDefault Local model.authentication.method of
                    Blockstack ->
                        text "Blockstack"

                    Local ->
                        text "anonymous"

                    RemoteStorage ->
                        text "RemoteStorage"
                , text " authentication mode."
                ]

            --
            , theForm model
            ]
        ]


theForm : Model -> Node
theForm model =
    let
        chosenBackdrop =
            Maybe.withDefault defaultBackdrop model.settings.chosenBackdrop
    in
        column
            Zed
            [ spacing (scaled -10), width (percent 50) ]
            [ -----------------------------------
              -- Background image
              -----------------------------------
              Layouts.lbl "Background Image"
            , Layouts.select backgroundImageMsg chosenBackdrop backgroundImages
            ]


backgroundImages : List ( String, String )
backgroundImages =
    [ ( "1.jpg", "Option 1" )
    , ( "2.jpg", "Option 2" )
    , ( "3.jpg", "Option 3" )
    , ( "4.jpg", "Option 4" )
    , ( "5.jpg", "Option 5" )
    , ( "6.jpg", "Option 6" )
    , ( "7.jpg", "Option 7 (default)" )
    , ( "8.jpg", "Option 8" )
    , ( "9.jpg", "Option 9" )
    , ( "10.jpg", "Option 10" )
    , ( "11.jpg", "Option 11" )
    , ( "12.jpg", "Option 12" )
    ]


backgroundImageMsg : String -> TopLevel.Msg
backgroundImageMsg =
    SetChosenBackdrop >> SettingsMsg
