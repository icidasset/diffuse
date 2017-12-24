module Settings.View exposing (..)

import Authentication.Types as Authentication exposing (Method(..))
import Layouts
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types exposing (Page(Abroad))
import Settings.Types
import Types exposing (Model, Msg(..))
import Variables exposing (scaled)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Ext
import Element.Input as Input
import Element.Types exposing (Node)


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
          Navigation.insideCustomNew
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
            , model
                |> theForm
                |> Element.map SettingsMsg
            ]
        ]


theForm : Model -> Element Styles variations Settings.Types.Msg
theForm model =
    column
        Zed
        [ width (percent 50) ]
        [ -----------------------------------
          -- Background image
          -----------------------------------
          Input.select (Styles.Form Form.Styles.Input)
            [ paddingXY 0 (scaled -5)
            , spacingXY 0 (scaled -10)
            ]
            { max = 1
            , options = []
            , with = model.settings.backgroundImage

            -- Label
            --
            , label =
                "Background image"
                    |> text
                    |> el (Styles.Form Form.Styles.Label) []
                    |> Input.labelAbove

            -- Menu
            --
            , menu =
                Input.menu (Styles.Form Form.Styles.Select)
                    []
                    (List.map
                        (\( v, l ) -> Input.choice v (text l))
                        backgroundImages
                    )
            }
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
    ]
