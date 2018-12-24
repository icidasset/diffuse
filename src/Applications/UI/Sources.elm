module UI.Sources exposing (Model, Msg(..), initialModel, update, view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Replying exposing (R3D3)
import Return2
import Return3
import Sources exposing (..)
import Sources.Services as Services
import Tachyons.Classes as T
import UI.Kit exposing (ButtonType(..), select)
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Reply exposing (Reply(..))
import UI.Sources.Form as Form



-- 🌳


type alias Model =
    { form : Form.Model }


initialModel : Model
initialModel =
    { form = Form.initialModel
    }



-- 📣


type Msg
    = Bypass
      -----------------------------------------
      -- Children
      -----------------------------------------
    | FormMsg Form.Msg


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            model
                |> Return3.withNothing

        -----------------------------------------
        -- Children
        -----------------------------------------
        FormMsg sub ->
            model.form
                |> Form.update sub
                |> Return2.mapModel (\f -> { model | form = f })
                |> Return2.mapCmd FormMsg
                |> Return3.withNoReply



-- 🗺


view : Sources.Page -> Model -> Html Msg
view page model =
    UI.Kit.vessel
        (case page of
            Index ->
                index model

            New ->
                List.map (Html.map FormMsg) (Form.new model.form)
        )



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
