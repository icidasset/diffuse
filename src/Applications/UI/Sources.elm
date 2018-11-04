module UI.Sources exposing (Model, Msg(..), initialModel, update, view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Replying exposing (R3D3)
import Sources exposing (..)
import Tachyons.Classes as T
import UI.Kit exposing (ButtonType(..), select)
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Reply exposing (Reply(..))



-- 🌳


type alias Model =
    { form : Sources.Form }


initialModel : Model
initialModel =
    { form = Sources.newForm
    }



-- 📣


type Msg
    = Bypass


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    ( model
    , Cmd.none
    , Nothing
    )



-- 🗺


view : Sources.Page -> Model -> Html Msg
view page =
    case page of
        Index ->
            UI.Kit.vessel << index

        New ->
            UI.Kit.vessel << new



-- INDEX


index : Model -> List (Html Msg)
index model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.add
          , Label "Add a new source" Shown
          , GoToPage (Page.Sources New)
          )
        , ( Icon Icons.sync
          , Label "Process sources" Shown
          , PerformMsg Bypass
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , UI.Kit.canister
        [ UI.Kit.h1 "Sources"
        , [ text "A source is a place where your music is stored."
          , lineBreak
          , text "By connecting a source, the application will scan it and keep a list of all the music in it."
          , lineBreak
          , text "It will not copy anything."
          ]
            |> Html.span []
            |> UI.Kit.intro
        ]
    ]



-- NEW


new : Model -> List (Html Msg)
new model =
    case model.form.step of
        Where ->
            newWhere model.form

        _ ->
            [ empty ]


newWhere : Form -> List (Html Msg)
newWhere { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Back to list" Hidden
          , GoToPage (Page.Sources Sources.Index)
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , chunk
        [ T.flex
        , T.flex_grow_1
        , T.overflow_hidden
        , T.relative
        ]
        [ UI.Kit.logoBackdrop
        , chunk
            [ T.flex
            , T.flex_column
            , T.flex_grow_1
            , T.items_center
            , T.justify_center
            , T.relative
            , T.z_1
            ]
            [ UI.Kit.h2 "Where is your music stored?"

            --
            , [ Html.option
                    [ value "AmazonS3" ]
                    [ text "Amazon S3" ]
              ]
                |> select (always Bypass)
                |> chunky [ T.pv2, T.w_100 ]

            --
            , UI.Kit.button
                WithIcon
                Bypass
                (Icons.arrow_forward UI.Kit.colorKit.base0B 17)
            ]
        ]
    ]
