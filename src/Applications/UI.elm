module UI exposing (main)

import Alien
import Authentication
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Common exposing (Switch(..))
import Css exposing (url)
import Css.Global
import Dict.Ext as Dict
import File
import File.Download
import File.Select
import Html.Styled as Html exposing (Html, div, section, text, toUnstyled)
import Html.Styled.Attributes exposing (id, style)
import Html.Styled.Lazy as Lazy
import Json.Decode
import Json.Encode
import Notifications
import Process
import Replying as N5 exposing (do, return)
import Return2 as R2
import Sources
import Sources.Encoding
import Tachyons.Classes as T
import Task
import Time
import Tracks.Encoding
import UI.Authentication as Authentication
import UI.Backdrop as Backdrop
import UI.Console
import UI.Core as Core exposing (Flags, Model, Msg(..))
import UI.Kit
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.Common
import UI.Queue.Core as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Settings as Settings
import UI.Settings.Page
import UI.Sources as Sources
import UI.Sources.Page
import UI.Svg.Elements
import UI.Tracks as Tracks
import UI.Tracks.Core as Tracks
import UI.UserData as UserData
import Url exposing (Url)



-- ⛩


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- 🌳


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { currentTime = Time.millisToPosix flags.initialTime
      , isAuthenticated = False
      , isLoading = True
      , navKey = key
      , notifications = []
      , page = Maybe.withDefault Page.Index (Page.fromUrl url)
      , url = url
      , viewport = flags.viewport

      -- Audio
      --------
      , audioDuration = 0
      , audioHasStalled = False
      , audioIsLoading = False
      , audioIsPlaying = False

      -- Children
      -----------
      , authentication = Authentication.initialModel
      , backdrop = Backdrop.initialModel
      , queue = Queue.initialModel
      , sources = Sources.initialModel
      , tracks = Tracks.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , case Page.fromUrl url of
        Just _ ->
            Cmd.none

        Nothing ->
            Nav.replaceUrl key "/"
    )



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            R2.withNoCmd model

        LoadEnclosedUserData json ->
            model
                |> UserData.importEnclosed json
                |> N5.reducto update translateReply

        LoadHypaethralUserData json ->
            { model | isAuthenticated = True }
                |> UserData.importHypaethral json
                |> N5.reducto update translateReply

        SetCurrentTime time ->
            let
                sources =
                    model.sources
            in
            ( { model
                | currentTime = time
                , sources = { sources | currentTime = time }
              }
            , Cmd.none
            )

        Core.ToggleLoadingScreen On ->
            R2.withNoCmd { model | isLoading = True }

        Core.ToggleLoadingScreen Off ->
            R2.withNoCmd { model | isLoading = False }

        -----------------------------------------
        -- Audio
        -----------------------------------------
        Pause ->
            R2.withCmd (Ports.pause ()) model

        Play ->
            R2.withCmd (Ports.play ()) model

        Seek percentage ->
            R2.withCmd (Ports.seek percentage) model

        SetAudioDuration duration ->
            R2.withNoCmd { model | audioDuration = duration }

        SetAudioHasStalled hasStalled ->
            R2.withNoCmd { model | audioHasStalled = hasStalled }

        SetAudioIsLoading isLoading ->
            R2.withNoCmd { model | audioIsLoading = isLoading }

        SetAudioIsPlaying isPlayinh ->
            R2.withNoCmd { model | audioIsPlaying = isPlayinh }

        Unstall ->
            R2.withCmd (Ports.unstall ()) model

        -----------------------------------------
        -- Brain
        -----------------------------------------
        Core.ProcessSources ->
            ( model
            , [ ( "origin"
                , Json.Encode.string (Common.urlOrigin model.url)
                )
              , ( "sources"
                , Json.Encode.list Sources.Encoding.encode model.sources.collection
                )
              , ( "tracks"
                , Json.Encode.list Tracks.Encoding.encodeTrack model.tracks.collection.untouched
                )
              ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.ProcessSources
                |> Ports.toBrain
            )

        Core.SaveEnclosedUserData ->
            model
                |> UserData.exportEnclosed
                |> Alien.broadcast Alien.SaveEnclosedUserData
                |> Ports.toBrain
                |> R2.withModel model

        Core.SaveFavourites ->
            model
                |> UserData.encodedFavourites
                |> Alien.broadcast Alien.SaveFavourites
                |> Ports.toBrain
                |> R2.withModel model

        Core.SaveSources ->
            let
                updateEnabledSourceIdsOnTracks =
                    model.sources.collection
                        |> Sources.enabledSourceIds
                        |> Tracks.SetEnabledSourceIds
                        |> TracksMsg
                        |> update

                ( updatedModel, updatedCmd ) =
                    updateEnabledSourceIdsOnTracks model
            in
            updatedModel
                |> UserData.encodedSources
                |> Alien.broadcast Alien.SaveSources
                |> Ports.toBrain
                |> R2.withModel updatedModel
                |> R2.addCmd updatedCmd

        Core.SaveTracks ->
            model
                |> UserData.encodedTracks
                |> Alien.broadcast Alien.SaveTracks
                |> Ports.toBrain
                |> R2.withModel model

        Core.SignOut ->
            let
                alienSigningOut =
                    Alien.SignOut
                        |> Alien.trigger
                        |> Ports.toBrain
            in
            { model
                | isAuthenticated = False
                , sources = Sources.initialModel
                , tracks = Tracks.initialModel
            }
                |> update (AuthenticationMsg Authentication.DischargeMethod)
                |> R2.addCmd alienSigningOut
                |> R2.addCmd (Nav.pushUrl model.navKey "/")

        -----------------------------------------
        -- Children
        -----------------------------------------
        AuthenticationMsg sub ->
            updateChild
                { mapCmd = AuthenticationMsg
                , mapModel = \child -> { model | authentication = child }
                , update = Authentication.update
                }
                { model = model.authentication
                , msg = sub
                }

        BackdropMsg sub ->
            updateChild
                { mapCmd = BackdropMsg
                , mapModel = \child -> { model | backdrop = child }
                , update = Backdrop.update
                }
                { model = model.backdrop
                , msg = sub
                }

        QueueMsg sub ->
            updateChild
                { mapCmd = QueueMsg
                , mapModel = \child -> { model | queue = child }
                , update = Queue.update
                }
                { model = model.queue
                , msg = sub
                }

        SourcesMsg sub ->
            updateChild
                { mapCmd = SourcesMsg
                , mapModel = \child -> { model | sources = child }
                , update = Sources.update
                }
                { model = model.sources
                , msg = sub
                }

        TracksMsg sub ->
            updateChild
                { mapCmd = TracksMsg
                , mapModel = \child -> { model | tracks = child }
                , update = Tracks.update
                }
                { model = model.tracks
                , msg = sub
                }

        -----------------------------------------
        -- Children, Pt. 2
        -----------------------------------------
        Core.ActiveQueueItemChanged maybeQueueItem ->
            let
                nowPlaying =
                    Maybe.map .identifiedTrack maybeQueueItem

                portCmd =
                    maybeQueueItem
                        |> Maybe.map .identifiedTrack
                        |> Maybe.map
                            (UI.Queue.Common.makeEngineItem
                                model.currentTime
                                model.sources.collection
                            )
                        |> Ports.activeQueueItemChanged
            in
            model
                |> update (TracksMsg <| Tracks.SetNowPlaying nowPlaying)
                |> R2.addCmd portCmd

        Core.FillQueue ->
            update
                (model.tracks.collection.harvested
                    |> Queue.Fill model.currentTime
                    |> QueueMsg
                )
                model

        -----------------------------------------
        -- Import / Export
        -----------------------------------------
        Core.Export ->
            ( model
            , File.Download.string
                "diffuse.json"
                "application/json"
                ({ favourites = model.tracks.favourites
                 , sources = model.sources.collection
                 , tracks = model.tracks.collection.untouched
                 }
                    |> Authentication.encodeHypaethral
                    |> Json.Encode.encode 2
                )
            )

        Core.Import file ->
            ( { model | isLoading = True }
            , 250
                |> Process.sleep
                |> Task.andThen (\_ -> File.toString file)
                |> Task.perform ImportJson
            )

        Core.ImportJson json ->
            let
                notification =
                    Notifications.success "Imported data successfully!"
            in
            model
                |> update
                    (json
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.withDefault Json.Encode.null
                        |> LoadHypaethralUserData
                    )
                |> N5.andThen2 (update Core.SaveFavourites)
                |> N5.andThen2 (update Core.SaveSources)
                |> N5.andThen2 (update Core.SaveTracks)
                |> N5.andThen2 (update <| ShowNotification notification)
                |> R2.addCmd (do <| ChangeUrlUsingPage Page.Index)

        Core.InsertDemo ->
            model
                |> update (LoadHypaethralUserData UserData.demo)
                |> N5.andThen2 (update Core.SaveFavourites)
                |> N5.andThen2 (update Core.SaveSources)
                |> N5.andThen2 (update Core.SaveTracks)

        Core.RequestImport ->
            ( model
            , File.Select.file [ "application/json" ] Import
            )

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        DismissNotification args ->
            UI.Notifications.dismissNotification model args

        RemoveNotification { id } ->
            ( { model
                | notifications =
                    List.filter
                        (Notifications.id >> (/=) id)
                        model.notifications
              }
            , Cmd.none
            )

        ShowNotification notification ->
            UI.Notifications.showNotification model notification

        -----------------------------------------
        -- URL
        -----------------------------------------
        ChangeUrlUsingPage page ->
            ( model
            , Nav.pushUrl model.navKey (Page.toString page)
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if url.path == "/about" then
                        return model (Nav.load "/about")

                    else
                        return model (Nav.pushUrl model.navKey <| Url.toString url)

                Browser.External href ->
                    return model (Nav.load href)

        UrlChanged url ->
            case Page.fromUrl url of
                Just page ->
                    ( { model
                        | page = page
                        , url = url
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Nav.replaceUrl model.navKey "/"
                    )



-- 📣  ░░  CHILDREN & REPLIES


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        Reply.ActiveQueueItemChanged m ->
            Core.ActiveQueueItemChanged m

        Reply.AddSourceToCollection source ->
            SourcesMsg (Sources.AddToCollection source)

        Reply.Chill ->
            Bypass

        Reply.FillQueue ->
            Core.FillQueue

        Reply.GoToPage page ->
            ChangeUrlUsingPage page

        Reply.InsertDemo ->
            Core.InsertDemo

        Reply.PlayTrack identifiedTrack ->
            QueueMsg (Queue.InjectFirstAndPlay identifiedTrack)

        Reply.ProcessSources ->
            Core.ProcessSources

        Reply.RemoveTracksWithSourceId sourceId ->
            TracksMsg (Tracks.RemoveBySourceId sourceId)

        Reply.ResetQueue ->
            QueueMsg Queue.Reset

        Reply.ShiftQueue ->
            QueueMsg Queue.Shift

        Reply.SaveEnclosedUserData ->
            Core.SaveEnclosedUserData

        Reply.SaveFavourites ->
            Core.SaveFavourites

        Reply.SaveSources ->
            Core.SaveSources

        Reply.SaveTracks ->
            Core.SaveTracks

        Reply.ToggleLoadingScreen state ->
            Core.ToggleLoadingScreen state


updateChild =
    N5.updateChild update translateReply



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.fromBrain alien

        -- Audio
        --------
        , Ports.activeQueueItemEnded (QueueMsg << always Queue.Shift)
        , Ports.setAudioDuration SetAudioDuration
        , Ports.setAudioHasStalled SetAudioHasStalled
        , Ports.setAudioIsLoading SetAudioIsLoading
        , Ports.setAudioIsPlaying SetAudioIsPlaying

        --
        , Time.every (60 * 1000) SetCurrentTime
        ]


alien : Alien.Event -> Msg
alien event =
    case event.error of
        Nothing ->
            translateAlienData event

        Just err ->
            translateAlienError event err


translateAlienData : Alien.Event -> Msg
translateAlienData event =
    case Alien.tagFromString event.tag of
        Just Alien.AddTracks ->
            TracksMsg (Tracks.Add event.data)

        Just Alien.AuthMethod ->
            -- My brain told me which auth method we're using,
            -- so we can tell the user in the UI.
            AuthenticationMsg (Authentication.ActivateMethod event.data)

        Just Alien.FinishedProcessingSources ->
            SourcesMsg Sources.FinishedProcessing

        Just Alien.HideLoadingScreen ->
            Core.ToggleLoadingScreen Off

        Just Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData event.data

        Just Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData event.data

        Just Alien.RemoveTracksByPath ->
            TracksMsg (Tracks.RemoveByPaths event.data)

        Just Alien.ReportProcessingError ->
            case Json.Decode.decodeValue (Json.Decode.dict Json.Decode.string) event.data of
                Ok dict ->
                    ShowNotification
                        (Notifications.stickyError
                            ("Could not process the **"
                                ++ Dict.fetch "sourceName" "" dict
                                ++ "** source. I got the following response from the source:"
                            )
                            (Dict.fetch "error" "missingError" dict)
                            []
                        )

                Err _ ->
                    ShowNotification
                        (Notifications.error "Could not decode processing error")

        Just Alien.SearchTracks ->
            TracksMsg (Tracks.SetSearchResults event.data)

        Just Alien.UpdateSourceData ->
            SourcesMsg (Sources.UpdateSourceData event.data)

        _ ->
            Bypass


translateAlienError : Alien.Event -> String -> Msg
translateAlienError event err =
    case Alien.tagFromString event.tag of
        Just tag ->
            []
                |> Notifications.stickyError err ""
                |> ShowNotification

        Nothing ->
            Bypass



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ toUnstyled (body model) ]
    }


body : Model -> Html Msg
body model =
    section
        []
        [ Css.Global.global globalCss

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , model.backdrop
            |> Lazy.lazy Backdrop.view
            |> Html.map BackdropMsg

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , model.notifications
            |> Lazy.lazy UI.Notifications.view

        -----------------------------------------
        -- Content
        -----------------------------------------
        , content
            (if model.isLoading then
                [ loadingAnimation ]

             else if model.isAuthenticated then
                defaultScreen model

             else
                [ model.authentication
                    |> Authentication.view
                    |> Html.map AuthenticationMsg
                ]
            )
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ Lazy.lazy
        (Navigation.global
            [ ( Page.Index, "Tracks" )
            , ( Page.Sources UI.Sources.Page.Index, "Sources" )
            , ( Page.Settings UI.Settings.Page.Index, "Settings" )
            ]
        )
        model.page

    -----------------------------------------
    -- Main
    -----------------------------------------
    , UI.Kit.vessel
        [ model
            |> Tracks.view
            |> Html.map TracksMsg

        -- Pages
        --------
        , case model.page of
            Page.Index ->
                nothing

            Page.Queue _ ->
                -- TODO
                nothing

            Page.Settings subPage ->
                Settings.view subPage model

            Page.Sources subPage ->
                model.sources
                    |> Lazy.lazy2 Sources.view subPage
                    |> Html.map SourcesMsg
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , UI.Console.view
        model.queue.activeItem
        model.queue.repeat
        model.queue.shuffle
        model.audioHasStalled
        model.audioIsLoading
        model.audioIsPlaying
    ]



-- 🗺  ░░  BITS


content : List (Html msg) -> Html msg
content =
    chunk
        [ T.flex
        , T.flex_column
        , T.items_center
        , T.justify_center
        , T.min_vh_100
        , T.ph3
        , T.relative
        , T.z_1
        ]


loadingAnimation : Html msg
loadingAnimation =
    Html.map never UI.Svg.Elements.loading



-- 🖼  ░░  GLOBAL


globalCss : List Css.Global.Snippet
globalCss =
    [ -----------------------------------------
      -- Body
      -----------------------------------------
      Css.Global.body
        [ Css.color (Color.toElmCssColor UI.Kit.colors.text)
        , Css.fontFamilies UI.Kit.defaultFontFamilies
        , Css.textRendering Css.optimizeLegibility

        -- Font smoothing
        -----------------
        , Css.property "-webkit-font-smoothing" "antialiased"
        , Css.property "-moz-osx-font-smoothing" "grayscale"
        , Css.property "font-smoothing" "antialiased"
        ]

    -----------------------------------------
    -- Placeholders
    -----------------------------------------
    , Css.Global.selector "::-webkit-input-placeholder" placeholderStyles
    , Css.Global.selector "::-moz-placeholder" placeholderStyles
    , Css.Global.selector ":-ms-input-placeholder" placeholderStyles
    , Css.Global.selector ":-moz-placeholder" placeholderStyles
    , Css.Global.selector "::placeholder" placeholderStyles
    ]


placeholderStyles : List Css.Style
placeholderStyles =
    [ Css.color (Css.rgb 0 0 0)
    , Css.opacity (Css.num 0.2)
    ]
