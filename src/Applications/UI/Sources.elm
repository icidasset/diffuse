module UI.Sources exposing (Model, Msg(..), initialModel, update, view)

import Chunky exposing (..)
import Dict.Ext as Dict
import Html.Styled as Html exposing (Html, text)
import Material.Icons.Action as Icons
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Replying exposing (R3D3)
import Return2
import Return3
import Sources exposing (..)
import Sources.Services as Services
import Tachyons.Classes as T
import Time
import UI.Kit exposing (ButtonType(..), select)
import UI.List
import UI.Navigation exposing (..)
import UI.Page
import UI.Reply exposing (Reply)
import UI.Sources.Form as Form
import UI.Sources.Page as Sources exposing (..)



-- 🌳


type alias Model =
    { collection : List Source
    , currentTime : Time.Posix
    , form : Form.Model
    , isProcessing : Bool
    }


initialModel : Model
initialModel =
    { collection = []
    , currentTime = Time.millisToPosix 0
    , form = Form.initialModel
    , isProcessing = False
    }



-- 📣


type Msg
    = Bypass
    | FinishedProcessing
    | Process
      -----------------------------------------
      -- Children
      -----------------------------------------
    | FormMsg Form.Msg
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | AddToCollection Source
    | RemoveFromCollection String


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            Return3.withNothing model

        FinishedProcessing ->
            ( { model | isProcessing = False }
            , Cmd.none
            , Nothing
            )

        Process ->
            ( { model | isProcessing = True }
            , Cmd.none
            , Just [ UI.Reply.ProcessSources ]
            )

        -----------------------------------------
        -- Children
        -----------------------------------------
        FormMsg sub ->
            model.form
                |> Form.update sub
                |> Return3.mapModel (\f -> { model | form = f })
                |> Return3.mapCmd FormMsg

        -----------------------------------------
        -- Collection
        -----------------------------------------
        AddToCollection source ->
            source
                |> setProperId (List.length model.collection + 1) model.currentTime
                |> List.singleton
                |> List.append model.collection
                |> (\c -> { model | collection = c, isProcessing = True })
                |> Return2.withNoCmd
                |> Return3.withReply
                    [ UI.Reply.SaveSources
                    , UI.Reply.ProcessSources
                    ]

        RemoveFromCollection sourceId ->
            model.collection
                |> List.filter (.id >> (/=) sourceId)
                |> (\c -> { model | collection = c })
                |> Return2.withNoCmd
                |> Return3.withReply
                    [ UI.Reply.SaveSources
                    , UI.Reply.RemoveTracksWithSourceId sourceId
                    ]



-- 🗺


view : Sources.Page -> Model -> Html Msg
view page model =
    UI.Kit.receptacle
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
          , GoToPage (UI.Page.Sources New)
          )

        -- Process
        ----------
        , if model.isProcessing then
            ( Icon Icons.sync
            , Label "Processing sources ..." Shown
            , PerformMsg Bypass
            )

          else
            ( Icon Icons.sync
            , Label "Process sources" Shown
            , PerformMsg Process
            )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , UI.Kit.canister
        [ UI.Kit.h1 "Sources"

        -- Intro
        --------
        , [ text "A source is a place where your music is stored."
          , lineBreak
          , text "By connecting a source, the application will scan it and keep a list of all the music in it."
          , lineBreak
          , text "It will not copy anything."
          ]
            |> raw
            |> UI.Kit.intro

        -- List
        -------
        , model.collection
            |> List.map (\s -> { label = Dict.fetch "name" "" s.data, actions = sourceActions s })
            |> UI.List.view
        ]
    ]


sourceActions : Source -> List (UI.List.Action Msg)
sourceActions source =
    [ { icon = Icons.close
      , msg = RemoveFromCollection source.id
      }
    ]
