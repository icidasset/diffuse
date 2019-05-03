module UI.Sources exposing (Model, Msg(..), initialModel, update, view)

import Chunky exposing (..)
import Conditional exposing (ifThenElse)
import Coordinates exposing (Coordinates)
import Dict.Ext as Dict
import Html.Events.Extra.Mouse as Mouse
import Html.Styled as Html exposing (Html, text)
import Json.Decode as Json
import Material.Icons exposing (Coloring(..))
import Material.Icons.Action as Icons
import Material.Icons.Alert as Icons
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Return3 as Return exposing (..)
import Sources exposing (..)
import Sources.Encoding
import Time
import UI.Kit exposing (ButtonType(..))
import UI.List
import UI.Navigation exposing (..)
import UI.Page
import UI.Reply exposing (Reply(..))
import UI.Sources.Form as Form
import UI.Sources.Page as Sources exposing (..)



-- 🌳


type alias Model =
    { collection : List Source
    , currentTime : Time.Posix
    , form : Form.Model
    , isProcessing : Bool
    , processingError : Maybe { error : String, sourceId : String }
    , processingNotificationId : Maybe Int
    }


initialModel : Model
initialModel =
    { collection = []
    , currentTime = Time.millisToPosix 0
    , form = Form.initialModel
    , isProcessing = False
    , processingError = Nothing
    , processingNotificationId = Nothing
    }



-- 📣


type Msg
    = Bypass
    | FinishedProcessing
    | Process
    | ReportProcessingError Json.Value
      -----------------------------------------
      -- Children
      -----------------------------------------
    | FormMsg Form.Msg
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | AddToCollection Source
    | RemoveFromCollection { sourceId : String }
    | UpdateSourceData Json.Value
      -----------------------------------------
      -- Individual
      -----------------------------------------
    | SourceContextMenu Source Mouse.Event
    | ToggleActivation { sourceId : String }
    | ToggleDirectoryPlaylists { sourceId : String }


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            return model

        FinishedProcessing ->
            model.processingNotificationId
                |> Maybe.map (\id -> [ DismissNotification { id = id } ])
                |> Maybe.withDefault []
                |> Return.repliesWithModel { model | isProcessing = False }

        Process ->
            if List.isEmpty model.collection then
                return model

            else
                returnReplyWithModel { model | isProcessing = True } ProcessSources

        ReportProcessingError json ->
            case Json.decodeValue (Json.dict Json.string) json of
                Ok dict ->
                    let
                        args =
                            { error = Dict.fetch "error" "" dict
                            , sourceId = Dict.fetch "sourceId" "" dict
                            }
                    in
                    dict
                        |> Dict.fetch "error" "missingError"
                        |> ShowErrorNotificationWithCode
                            ("Could not process the _"
                                ++ Dict.fetch "sourceName\n                                " "" dict
                                ++ "_ source. I got the following response from the source:"
                            )
                        |> returnReplyWithModel
                            { model | processingError = Just args }

                Err _ ->
                    "Could not decode processing error"
                        |> ShowErrorNotification
                        |> returnReplyWithModel model

        -----------------------------------------
        -- Children
        -----------------------------------------
        FormMsg sub ->
            model.form
                |> Form.update sub
                |> mapModel (\f -> { model | form = f })
                |> mapCmd FormMsg

        -----------------------------------------
        -- Collection
        -----------------------------------------
        AddToCollection source ->
            source
                |> setProperId (List.length model.collection + 1) model.currentTime
                |> List.singleton
                |> List.append model.collection
                |> (\c -> { model | collection = c, isProcessing = True })
                |> return
                |> addReplies
                    [ UI.Reply.SaveSources
                    , UI.Reply.ProcessSources
                    ]

        RemoveFromCollection { sourceId } ->
            model.collection
                |> List.filter (.id >> (/=) sourceId)
                |> (\c -> { model | collection = c })
                |> return
                |> addReplies
                    [ UI.Reply.SaveSources
                    , UI.Reply.RemoveTracksWithSourceId sourceId
                    ]

        UpdateSourceData json ->
            json
                |> Sources.Encoding.decode
                |> Maybe.map
                    (\source ->
                        List.map
                            (\s ->
                                if s.id == source.id then
                                    source

                                else
                                    s
                            )
                            model.collection
                    )
                |> Maybe.map (\col -> { model | collection = col })
                |> Maybe.withDefault model
                |> return
                |> addReply UI.Reply.SaveSources

        -----------------------------------------
        -- Individual
        -----------------------------------------
        SourceContextMenu source mouseEvent ->
            let
                coordinates =
                    Coordinates.fromTuple mouseEvent.clientPos
            in
            returnRepliesWithModel
                model
                [ ShowSourceContextMenu coordinates source ]

        ToggleActivation { sourceId } ->
            model.collection
                |> List.map
                    (\source ->
                        if source.id == sourceId then
                            { source | enabled = not source.enabled }

                        else
                            source
                    )
                |> (\collection -> { model | collection = collection })
                |> return
                |> addReply SaveSources

        ToggleDirectoryPlaylists { sourceId } ->
            model.collection
                |> List.map
                    (\source ->
                        if source.id == sourceId then
                            { source | directoryPlaylists = not source.directoryPlaylists }

                        else
                            source
                    )
                |> (\collection -> { model | collection = collection })
                |> return
                |> addReply SaveSources



-- 🗺


view : Sources.Page -> Model -> Html Msg
view page model =
    UI.Kit.receptacle
        (case page of
            Index ->
                index model

            Edit sourceId ->
                List.map (Html.map FormMsg) (Form.edit model.form)

            New ->
                List.map (Html.map FormMsg) (Form.new model.form)

            NewThroughRedirect _ _ ->
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
          , NavigateToPage (UI.Page.Sources New)
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
            |> List.sortBy
                (.data >> Dict.fetch "name" "")
            |> List.map
                (\source ->
                    { label = Html.text (Dict.fetch "name" "" source.data)
                    , actions = sourceActions model.processingError source
                    }
                )
            |> UI.List.view
        ]
    ]


sourceActions : Maybe { error : String, sourceId : String } -> Source -> List (UI.List.Action Msg)
sourceActions processingError source =
    List.append
        (case processingError of
            Just { error, sourceId } ->
                if sourceId == source.id then
                    [ { color = Color UI.Kit.colors.error
                      , icon = Icons.error_outline
                      , msg = Nothing
                      , title = error
                      }
                    ]

                else
                    []

            Nothing ->
                []
        )
        [ { color = Inherit
          , icon =
                if source.enabled then
                    Icons.check

                else
                    Icons.block
          , msg =
                { sourceId = source.id }
                    |> ToggleActivation
                    |> always
                    |> Just
          , title =
                if source.enabled then
                    "Enabled (click to disable)"

                else
                    "Disabled (click to enable)"
          }

        --
        , { color = Inherit
          , icon = Icons.more_vert
          , msg = Just (SourceContextMenu source)
          , title = "Menu"
          }
        ]
