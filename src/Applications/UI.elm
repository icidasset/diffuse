module UI exposing (main)

import Alien
import Authentication
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Common
import Css exposing (url)
import Css.Global
import Html.Styled as Html exposing (Html, div, section, text, toUnstyled)
import Html.Styled.Attributes exposing (id, style)
import Html.Styled.Lazy as Lazy
import Json.Encode as Encode
import Replying exposing (do, return)
import Return2
import Return3
import Sources
import Sources.Encoding
import Tachyons.Classes as T
import Time
import Tracks.Encoding
import UI.Authentication
import UI.Backdrop
import UI.Core as Core exposing (Flags, Model, Msg(..), Switch(..))
import UI.Kit
import UI.Navigation
import UI.Page as Page
import UI.Ports as Ports
import UI.Reply as Reply exposing (Reply(..))
import UI.Settings
import UI.Sources
import UI.Svg.Elements
import UI.Tracks
import UI.UserData
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
      { isAuthenticated = False
      , isLoading = True
      , navKey = key
      , page = Page.fromUrl url
      , url = url

      -- Children
      , authentication = UI.Authentication.initialModel
      , backdrop = UI.Backdrop.initialModel
      , sources = UI.Sources.initialModel
      , tracks = UI.Tracks.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.none
    )



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            ( model
            , Cmd.none
            )

        LoadEnclosedUserData json ->
            ( model
            , Cmd.none
            )

        LoadHypaethralUserData json ->
            { model | isAuthenticated = True, isLoading = False }
                |> UI.UserData.importHypaethral json
                |> Replying.reducto update translateReply

        SetCurrentTime time ->
            let
                sources =
                    model.sources
            in
            ( { model | sources = { sources | currentTime = time } }
            , Cmd.none
            )

        ToggleLoadingScreen On ->
            ( { model | isLoading = True }
            , Cmd.none
            )

        ToggleLoadingScreen Off ->
            ( { model | isLoading = False }
            , Cmd.none
            )

        -----------------------------------------
        -- Brain
        -----------------------------------------
        Core.ProcessSources ->
            ( model
            , [ ( "origin"
                , Encode.string (Common.urlOrigin model.url)
                )
              , ( "sources"
                , Encode.list Sources.Encoding.encode model.sources.collection
                )
              , ( "tracks"
                , Encode.list Tracks.Encoding.encodeTrack model.tracks.collection.untouched
                )
              ]
                |> Encode.object
                |> Alien.broadcast Alien.ProcessSources
                |> Ports.toBrain
            )

        Core.SaveEnclosedUserData ->
            ( model
            , Cmd.none
            )

        Core.SaveFavourites ->
            model
                |> UI.UserData.encodedFavourites
                |> Alien.broadcast Alien.SaveFavourites
                |> Ports.toBrain
                |> Return2.withModel model

        Core.SaveSources ->
            let
                updateEnabledSourceIdsOnTracks =
                    model.sources.collection
                        |> Sources.enabledSourceIds
                        |> UI.Tracks.SetEnabledSourceIds
                        |> TracksMsg
                        |> update

                ( updatedModel, updatedCmd ) =
                    updateEnabledSourceIdsOnTracks model
            in
            updatedModel
                |> UI.UserData.encodedSources
                |> Alien.broadcast Alien.SaveSources
                |> Ports.toBrain
                |> Return2.withModel updatedModel
                |> Return2.addCmd updatedCmd

        Core.SaveTracks ->
            model
                |> UI.UserData.encodedTracks
                |> Alien.broadcast Alien.SaveTracks
                |> Ports.toBrain
                |> Return2.withModel model

        SignOut ->
            -- TODO: Reset user data
            ( { model | isAuthenticated = False }
            , Alien.SignOut
                |> Alien.trigger
                |> Ports.toBrain
            )

        -----------------------------------------
        -- Children
        -----------------------------------------
        AuthenticationMsg sub ->
            updateChild
                { mapCmd = AuthenticationMsg
                , mapModel = \child -> { model | authentication = child }
                , update = UI.Authentication.update
                }
                { model = model.authentication
                , msg = sub
                }

        BackdropMsg sub ->
            updateChild
                { mapCmd = BackdropMsg
                , mapModel = \child -> { model | backdrop = child }
                , update = UI.Backdrop.update
                }
                { model = model.backdrop
                , msg = sub
                }

        SourcesMsg sub ->
            updateChild
                { mapCmd = SourcesMsg
                , mapModel = \child -> { model | sources = child }
                , update = UI.Sources.update
                }
                { model = model.sources
                , msg = sub
                }

        TracksMsg sub ->
            updateChild
                { mapCmd = TracksMsg
                , mapModel = \child -> { model | tracks = child }
                , update = UI.Tracks.update
                }
                { model = model.tracks
                , msg = sub
                }

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
            ( { model
                | page = Page.fromUrl url
                , url = url
              }
            , Cmd.none
            )



-- 📣  ░░  CHILDREN & REPLIES


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        AddSourceToCollection source ->
            SourcesMsg (UI.Sources.AddToCollection source)

        Chill ->
            Bypass

        GoToPage page ->
            ChangeUrlUsingPage page

        Reply.ProcessSources ->
            Core.ProcessSources

        Reply.RemoveTracksWithSourceId sourceId ->
            TracksMsg (UI.Tracks.RemoveBySourceId sourceId)

        Reply.SaveEnclosedUserData ->
            Core.SaveEnclosedUserData

        Reply.SaveFavourites ->
            Core.SaveFavourites

        Reply.SaveSources ->
            Core.SaveSources

        Reply.SaveTracks ->
            Core.SaveTracks


updateChild =
    Replying.updateChild update translateReply



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.fromBrain translateAlienEvent
        , Time.every (60 * 1000) SetCurrentTime
        ]


translateAlienEvent : Alien.Event -> Msg
translateAlienEvent event =
    case Alien.tagFromString event.tag of
        Just Alien.AddTracks ->
            TracksMsg (UI.Tracks.Add event.data)

        Just Alien.FinishedProcessingSources ->
            SourcesMsg UI.Sources.FinishedProcessing

        Just Alien.HideLoadingScreen ->
            ToggleLoadingScreen Off

        Just Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData event.data

        Just Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData event.data

        Just Alien.RemoveTracksByPath ->
            TracksMsg (UI.Tracks.RemoveByPaths event.data)

        Just Alien.ReportGenericError ->
            let
                dbg =
                    -- TODO
                    Debug.log "error" event
            in
            Bypass

        Just Alien.ReportProcessingError ->
            -- TODO
            Bypass

        Just Alien.SearchTracks ->
            TracksMsg (UI.Tracks.SetSearchResults event.data)

        Just Alien.UpdateSourceData ->
            -- TODO
            Bypass

        _ ->
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
            |> Lazy.lazy UI.Backdrop.view
            |> Html.map BackdropMsg

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
                    |> UI.Authentication.view
                    |> Html.map AuthenticationMsg
                ]
            )
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ Lazy.lazy
        (UI.Navigation.global
            [ ( Page.Index, "Tracks" )
            , ( Page.Sources Sources.Index, "Sources" )
            , ( Page.Settings, "Settings" )
            ]
        )
        model.page

    -----------------------------------------
    -- Main
    -----------------------------------------
    , UI.Kit.vessel
        [ model.tracks
            |> Lazy.lazy UI.Tracks.view
            |> Html.map TracksMsg

        -- Pages
        --------
        , case model.page of
            Page.Index ->
                empty

            Page.NotFound ->
                UI.Kit.receptacle [ text "Page not found." ]

            Page.Settings ->
                UI.Settings.view model

            Page.Sources subPage ->
                model.sources
                    |> Lazy.lazy2 UI.Sources.view subPage
                    |> Html.map SourcesMsg
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , chunk
        [ T.h4 ]
        []
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
