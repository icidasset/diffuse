module UI.Sources exposing (Model, Msg(..), initialModel, sourcesToProcess, update, view)

import Alien
import Chunky exposing (..)
import Conditional exposing (ifThenElse)
import Coordinates
import Css.Classes as C
import Dict.Ext as Dict
import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Json
import List.Extra as List
import Material.Icons exposing (Coloring(..))
import Material.Icons.Action as Icons
import Material.Icons.Alert as Icons
import Material.Icons.Content as Icons
import Material.Icons.Image as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Return3 as Return exposing (..)
import Sources exposing (..)
import Sources.Encoding
import Time
import Time.Ext as Time
import UI.Kit exposing (ButtonType(..))
import UI.List
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))
import UI.Sources.Form as Form
import UI.Sources.Page as Sources exposing (..)



-- 🌳


type alias Model =
    { collection : List Source
    , currentTime : Time.Posix
    , form : Form.Model
    , isProcessing : List ( String, Float )
    , processingError : Maybe { error : String, sourceId : String }
    , processingNotificationId : Maybe Int
    }


initialModel : Model
initialModel =
    { collection = []
    , currentTime = Time.default
    , form = Form.initialModel
    , isProcessing = []
    , processingError = Nothing
    , processingNotificationId = Nothing
    }



-- 📣


type Msg
    = Bypass
    | FinishedProcessingSource String
    | FinishedProcessing
    | Process
    | ReportProcessingError Json.Value
    | ReportProcessingProgress Json.Value
    | StopProcessing
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
                |> Return.repliesWithModel { model | isProcessing = [] }

        FinishedProcessingSource sourceId ->
            return { model | isProcessing = List.filter (Tuple.first >> (/=) sourceId) model.isProcessing }

        Process ->
            model
                |> sourcesToProcess
                |> ProcessSources
                |> returnReplyWithModel model

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
                        |> ShowStickyErrorNotificationWithCode
                            ("Could not process the _"
                                ++ Dict.fetch "sourceName" "" dict
                                ++ "_ source. I got the following response from the source:"
                            )
                        |> returnReplyWithModel
                            { model | processingError = Just args }

                Err _ ->
                    "Could not decode processing error"
                        |> ShowStickyErrorNotification
                        |> returnReplyWithModel model

        ReportProcessingProgress json ->
            case
                Json.decodeValue
                    (Json.map2
                        (\p s ->
                            { progress = p
                            , sourceId = s
                            }
                        )
                        (Json.field "progress" Json.float)
                        (Json.field "sourceId" Json.string)
                    )
                    json
            of
                Ok { progress, sourceId } ->
                    model.isProcessing
                        |> List.map
                            (\( sid, pro ) ->
                                ifThenElse (sid == sourceId)
                                    ( sid, progress )
                                    ( sid, pro )
                            )
                        |> (\isProcessing ->
                                { model | isProcessing = isProcessing }
                           )
                        |> return

                Err _ ->
                    "Could not decode processing progress"
                        |> ShowStickyErrorNotification
                        |> returnReplyWithModel model

        StopProcessing ->
            case model.processingNotificationId of
                Just notificationId ->
                    Alien.StopProcessing
                        |> Alien.trigger
                        |> Ports.toBrain
                        |> returnCommandWithModel
                            { model
                                | isProcessing = []
                                , processingNotificationId = Nothing
                            }
                        |> addReply (DismissNotification { id = notificationId })

                Nothing ->
                    return model

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
        AddToCollection unsuitableSource ->
            let
                source =
                    setProperId
                        (List.length model.collection + 1)
                        model.currentTime
                        unsuitableSource
            in
            returnRepliesWithModel
                { model | collection = model.collection ++ [ source ] }
                [ UI.Reply.SaveSources
                , UI.Reply.ProcessSources [ source ]
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
            returnRepliesWithModel
                model
                [ ShowSourceContextMenu
                    (Coordinates.fromTuple mouseEvent.clientPos)
                    source
                ]

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
                |> addReply GenerateDirectoryPlaylists


sourcesToProcess : Model -> List Source
sourcesToProcess model =
    List.filter (.enabled >> (==) True) model.collection



-- 🗺


view : { amountOfTracks : Int } -> Sources.Page -> Model -> Html Msg
view { amountOfTracks } page model =
    UI.Kit.receptacle
        { scrolling = True }
        (case page of
            Index ->
                index amountOfTracks model

            Edit sourceId ->
                List.map (Html.map FormMsg) (Form.edit model.form)

            New ->
                List.map (Html.map FormMsg) (Form.new { onboarding = False } model.form)

            NewOnboarding ->
                List.map (Html.map FormMsg) (Form.new { onboarding = True } model.form)

            NewThroughRedirect _ _ ->
                List.map (Html.map FormMsg) (Form.new { onboarding = False } model.form)
        )



-- INDEX


index : Int -> Model -> List (Html Msg)
index amountOfTracks model =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      if List.isEmpty model.collection then
        UI.Navigation.local
            [ ( Icon Icons.add
              , Label "Add a new source" Shown
              , NavigateToPage (Page.Sources New)
              )
            ]

      else
        UI.Navigation.local
            [ ( Icon Icons.add
              , Label "Add a new source" Shown
              , NavigateToPage (Page.Sources New)
              )

            -- Process
            ----------
            , if List.isEmpty model.isProcessing then
                ( Icon Icons.sync
                , Label "Process sources" Shown
                , PerformMsg Process
                )

              else
                ( Icon Icons.sync
                , Label "Stop processing ..." Shown
                , PerformMsg StopProcessing
                )
            ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , if List.isEmpty model.collection then
        chunk
            [ C.relative ]
            [ chunk
                [ C.absolute, C.left_0, C.top_0 ]
                [ UI.Kit.canister [ UI.Kit.h1 "Sources" ] ]
            ]

      else
        UI.Kit.canister
            [ UI.Kit.h1 "Sources"

            -- Intro
            --------
            , intro amountOfTracks

            -- List
            -------
            , model.collection
                |> List.sortBy
                    (.data >> Dict.fetch "name" "")
                |> List.map
                    (\source ->
                        { label = Html.text (Dict.fetch "name" "" source.data)
                        , actions = sourceActions model.isProcessing model.processingError source
                        , msg = Nothing
                        , isSelected = False
                        }
                    )
                |> UI.List.view UI.List.Normal
            ]

    --
    , if List.isEmpty model.collection then
        UI.Kit.centeredContent
            [ slab
                Html.a
                [ href (Page.toString <| Page.Sources New) ]
                [ C.block
                , C.opacity_30
                , C.text_inherit
                ]
                [ Icons.music_note 64 Inherit ]
            , slab
                Html.a
                [ href (Page.toString <| Page.Sources New) ]
                [ C.block
                , C.leading_normal
                , C.mt_2
                , C.opacity_40
                , C.text_center
                , C.text_inherit
                ]
                [ text "A source is a place where music is stored,"
                , lineBreak
                , text "add one so you can play some music "
                , inline
                    [ C.align_middle, C.inline_block, C.minus_mt_px ]
                    [ Icons.add 14 Inherit ]
                ]
            ]

      else
        nothing
    ]


intro : Int -> Html Msg
intro amountOfTracks =
    [ text "A source is a place where your music is stored."
    , lineBreak
    , text "By connecting a source, the application will scan it and keep a list of all the music in it."
    , lineBreak
    , text "You currently have "
    , text (String.fromInt amountOfTracks)
    , text " "
    , text (ifThenElse (amountOfTracks == 1) "track" "tracks")
    , text " in your collection."
    ]
        |> raw
        |> UI.Kit.intro


sourceActions : List ( String, Float ) -> Maybe { error : String, sourceId : String } -> Source -> List (UI.List.Action Msg)
sourceActions isProcessing processingError source =
    let
        processIndex =
            List.findIndex (Tuple.first >> (==) source.id) isProcessing

        process =
            Maybe.andThen (\idx -> List.getAt idx isProcessing) processIndex
    in
    List.append
        (case ( process, processingError ) of
            ( Just ( _, progress ), _ ) ->
                [ { color = Inherit
                  , icon =
                        \_ _ ->
                            if progress < 0.05 then
                                inline
                                    [ C.inline_block, C.opacity_70, C.px_1 ]
                                    [ case processIndex of
                                        Just 0 ->
                                            Html.text "Listing"

                                        _ ->
                                            Html.text "Waiting"
                                    ]

                            else
                                progress
                                    |> (*) 100
                                    |> round
                                    |> String.fromInt
                                    |> (\s -> s ++ "%")
                                    |> Html.text
                                    |> List.singleton
                                    |> inline [ C.inline_block, C.opacity_70, C.px_1 ]
                  , msg = Nothing
                  , title = ""
                  }
                , { color = Inherit
                  , icon = Icons.sync
                  , msg = Nothing
                  , title = "Currently processing"
                  }
                ]

            ( Nothing, Just { error, sourceId } ) ->
                if sourceId == source.id then
                    [ { color = Color UI.Kit.colors.error
                      , icon = Icons.error_outline
                      , msg = Nothing
                      , title = error
                      }
                    ]

                else
                    []

            _ ->
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
