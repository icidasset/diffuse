module Brain exposing (main)

import Alien
import Authentication exposing (HypaethralUserData)
import Brain.Authentication as Authentication
import Brain.Ports
import Brain.Reply exposing (Reply(..))
import Brain.Sources.Processing as Processing
import Brain.Sources.Processing.Common as Processing
import Brain.Tracks as Tracks
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Json.Decode as Json
import Json.Encode
import Maybe.Extra as Maybe
import Playlists.Encoding as Playlists
import Return2 exposing (..)
import Return3
import Sources.Encoding as Sources
import Sources.Processing as Processing
import Sources.Processing.Encoding as Processing
import Tracks
import Tracks.Encoding as Tracks
import Url



-- 🧠


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- 🌳


type alias Model =
    { authentication : Authentication.Model
    , hypaethralUserData : Authentication.HypaethralUserData
    , notSoFast : Debouncer Msg Msg
    , processing : Processing.Model
    , tracks : Tracks.Model
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { authentication = Authentication.initialModel
      , hypaethralUserData = Authentication.emptyHypaethralUserData
      , processing = Processing.initialModel
      , tracks = Tracks.initialModel

      --
      , notSoFast =
            2.5
                |> Debouncer.fromSeconds
                |> Debouncer.debounce
                |> Debouncer.toDebouncer
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.none
    )



-- 📣


type Msg
    = Bypass
    | Cmd (Cmd Msg)
    | Initialize String
    | NotifyUI Alien.Event
    | NotSoFast (Debouncer.Msg Msg)
    | Process Processing.Arguments
    | ToCache Alien.Event
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | RedirectToBlockstackSignIn
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AuthenticationMsg Authentication.Msg
    | ProcessingMsg Processing.Msg
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- User data
      -----------------------------------------
    | LoadHypaethralUserData Json.Value
    | RemoveTracksBySourceId String
    | SaveHypaethralData
    | SaveFavourites Json.Value
    | SavePlaylists Json.Value
    | SaveSettings Json.Value
    | SaveSources Json.Value
    | SaveTracks Json.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            return model

        Cmd cmd ->
            returnWithModel model cmd

        Initialize href ->
            let
                initialUrl =
                    Maybe.withDefault
                        { protocol = Url.Http
                        , host = ""
                        , port_ = Nothing
                        , path = ""
                        , query = Nothing
                        , fragment = Nothing
                        }
                        (Url.fromString href)
            in
            [ Cmd.map AuthenticationMsg (Authentication.initialCommand initialUrl)
            , Cmd.map ProcessingMsg Processing.initialCommand
            ]
                |> Cmd.batch
                |> returnWithModel model

        NotifyUI alienEvent ->
            [ Brain.Ports.toUI alienEvent

            -- Sometimes the loading screen is still showing,
            -- so we hide it here just in case.
            , case alienEvent.error of
                Just _ ->
                    Brain.Ports.toUI (Alien.trigger Alien.HideLoadingScreen)

                Nothing ->
                    Cmd.none
            ]
                |> Cmd.batch
                |> returnWithModel model

        NotSoFast debouncerMsg ->
            Return3.wieldNested
                update
                { mapCmd = NotSoFast
                , mapModel = \child -> { model | notSoFast = child }
                , update = \m -> Debouncer.update m >> Return3.fromDebouncer
                }
                { model = model.notSoFast
                , msg = debouncerMsg
                }

        Process { origin, sources } ->
            { origin = origin
            , sources = sources
            , tracks = model.hypaethralUserData.tracks
            }
                |> Processing.Process
                |> ProcessingMsg
                |> updateWithModel model

        ToCache alienEvent ->
            alienEvent
                |> Brain.Ports.toCache
                |> returnWithModel model

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        RedirectToBlockstackSignIn ->
            ()
                |> Brain.Ports.redirectToBlockstackSignIn
                |> returnWithModel model

        -----------------------------------------
        -- Children
        -----------------------------------------
        AuthenticationMsg Authentication.PerformSignOut ->
            -- When signing out, remove all traces of the user's data.
            updateAuthentication
                { model | hypaethralUserData = Authentication.emptyHypaethralUserData }
                Authentication.PerformSignOut

        AuthenticationMsg sub ->
            updateAuthentication model sub

        ProcessingMsg sub ->
            updateProcessing model sub

        TracksMsg sub ->
            updateTracks model sub

        -----------------------------------------
        -- User data
        -----------------------------------------
        --   The hypaethral user data is received in pieces,
        --   pieces which are "cached" here in the web worker.
        --
        --   The reasons for this are:
        --   1. Lesser performance penalty on the UI when saving data
        --      (ie. this avoids having to encode/decode everything each time)
        --   2. The data can be used in the web worker (brain) as well.
        --      (eg. for track-search index)
        --
        LoadHypaethralUserData value ->
            let
                decodedData =
                    value
                        |> Authentication.decodeHypaethral
                        |> Result.withDefault model.hypaethralUserData

                encodedTracks =
                    Json.Encode.list Tracks.encodeTrack decodedData.tracks
            in
            value
                |> Alien.broadcast Alien.LoadHypaethralUserData
                |> Brain.Ports.toUI
                |> returnWithModel { model | hypaethralUserData = decodedData }
                |> andThen (updateSearchIndex encodedTracks)

        RemoveTracksBySourceId sourceId ->
            model.hypaethralUserData.tracks
                |> Tracks.removeBySourceId sourceId
                |> .kept
                |> hypaethralLenses.setTracks model
                |> updateSearchIndexWithModel
                |> andThen saveHypaethralData

        SaveHypaethralData ->
            model.hypaethralUserData
                |> Authentication.encodeHypaethral
                |> Authentication.SaveHypaethralData
                |> AuthenticationMsg
                |> updateWithModel model

        SaveFavourites value ->
            value
                |> Json.decodeValue (Json.list Tracks.favouriteDecoder)
                |> Result.withDefault model.hypaethralUserData.favourites
                |> hypaethralLenses.setFavourites model
                |> saveHypaethralData

        SavePlaylists value ->
            value
                |> Json.decodeValue (Json.list Playlists.decoder)
                |> Result.withDefault model.hypaethralUserData.playlists
                |> hypaethralLenses.setPlaylists model
                |> saveHypaethralData

        SaveSettings value ->
            value
                |> Json.decodeValue (Json.map Just Authentication.settingsDecoder)
                |> Result.withDefault model.hypaethralUserData.settings
                |> hypaethralLenses.setSettings model
                |> saveHypaethralData

        SaveSources value ->
            value
                |> Json.decodeValue (Json.list Sources.decoder)
                |> Result.withDefault model.hypaethralUserData.sources
                |> hypaethralLenses.setSources model
                |> saveHypaethralData

        SaveTracks value ->
            value
                |> Json.decodeValue (Json.list Tracks.trackDecoder)
                |> Result.withDefault model.hypaethralUserData.tracks
                |> hypaethralLenses.setTracks model
                |> updateSearchIndex value
                |> andThen saveHypaethralData


updateWithModel : Model -> Msg -> ( Model, Cmd Msg )
updateWithModel model msg =
    update msg model


updateSearchIndex : Json.Value -> Model -> ( Model, Cmd Msg )
updateSearchIndex value model =
    value
        |> Tracks.UpdateSearchIndex
        |> TracksMsg
        |> updateWithModel model


updateSearchIndexWithModel : Model -> ( Model, Cmd Msg )
updateSearchIndexWithModel model =
    model.hypaethralUserData.tracks
        |> Json.Encode.list Tracks.encodeTrack
        |> Tracks.UpdateSearchIndex
        |> TracksMsg
        |> updateWithModel model



-- 📣  ░░  REPLIES


translateReply : Reply -> Model -> ( Model, Cmd Msg )
translateReply reply model =
    case reply of
        FabricatedNewSecretKey ->
            update SaveHypaethralData model

        -----------------------------------------
        -- Tracks
        -----------------------------------------
        AddTracks tracks ->
            tracks
                |> (++) model.hypaethralUserData.tracks
                |> hypaethralLenses.setTracks model
                |> updateSearchIndexWithModel
                |> andThen saveHypaethralData

        RemoveTracksByPaths args ->
            model.hypaethralUserData.tracks
                |> Tracks.removeByPaths args
                |> .kept
                |> hypaethralLenses.setTracks model
                |> updateSearchIndexWithModel
                |> andThen saveHypaethralData

        -----------------------------------------
        -- To UI
        -----------------------------------------
        GiveUI Alien.LoadHypaethralUserData data ->
            update (LoadHypaethralUserData data) model

        GiveUI tag data ->
            data
                |> Alien.broadcast tag
                |> NotifyUI
                |> updateWithModel model

        NudgeUI tag ->
            tag
                |> Alien.trigger
                |> NotifyUI
                |> updateWithModel model



-- 📣  ░░  CHILDREN


updateAuthentication : Model -> Authentication.Msg -> ( Model, Cmd Msg )
updateAuthentication model sub =
    Return3.wieldNested
        translateReply
        { mapCmd = AuthenticationMsg
        , mapModel = \child -> { model | authentication = child }
        , update = Authentication.update
        }
        { model = model.authentication
        , msg = sub
        }


updateProcessing : Model -> Processing.Msg -> ( Model, Cmd Msg )
updateProcessing model sub =
    Return3.wieldNested
        translateReply
        { mapCmd = ProcessingMsg
        , mapModel = \child -> { model | processing = child }
        , update = Processing.update
        }
        { model = model.processing
        , msg = sub
        }


updateTracks : Model -> Tracks.Msg -> ( Model, Cmd Msg )
updateTracks model sub =
    Return3.wieldNested
        translateReply
        { mapCmd = TracksMsg
        , mapModel = \child -> { model | tracks = child }
        , update = Tracks.update
        }
        { model = model.tracks
        , msg = sub
        }



-- 📣  ░░  USER DATA


hypaethralLenses =
    { setFavourites = makeHypaethralLens (\h f -> { h | favourites = f })
    , setPlaylists = makeHypaethralLens (\h p -> { h | playlists = p })
    , setSettings = makeHypaethralLens (\h s -> { h | settings = s })
    , setSources = makeHypaethralLens (\h s -> { h | sources = s })
    , setTracks = makeHypaethralLens (\h t -> { h | tracks = t })
    }


makeHypaethralLens : (HypaethralUserData -> a -> HypaethralUserData) -> Model -> a -> Model
makeHypaethralLens setter model value =
    { model | hypaethralUserData = setter model.hypaethralUserData value }


saveHypaethralData : Model -> ( Model, Cmd Msg )
saveHypaethralData model =
    SaveHypaethralData
        |> Debouncer.provideInput
        |> NotSoFast
        |> updateWithModel model



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Brain.Ports.fromAlien alien
        , Brain.Ports.initialize Initialize

        -----------------------------------------
        -- Children
        -----------------------------------------
        , Sub.map ProcessingMsg (Processing.subscriptions model.processing)
        , Sub.map TracksMsg (Tracks.subscriptions model.tracks)
        ]


alien : Alien.Event -> Msg
alien event =
    case ( event.error, Alien.tagFromString event.tag ) of
        ( Nothing, Just tag ) ->
            translateAlienData tag event.data

        ( Just err, Just tag ) ->
            translateAlienError tag err

        _ ->
            Bypass


translateAlienData : Alien.Tag -> Json.Value -> Msg
translateAlienData tag data =
    case tag of
        Alien.AuthAnonymous ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved data)

        Alien.AuthDropbox ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved data)

        Alien.AuthEnclosedData ->
            AuthenticationMsg (Authentication.EnclosedDataRetrieved data)

        Alien.AuthIpfs ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved data)

        Alien.AuthMethod ->
            AuthenticationMsg (Authentication.MethodRetrieved data)

        Alien.AuthRemoteStorage ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved data)

        Alien.AuthTextile ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved data)

        Alien.FabricateSecretKey ->
            AuthenticationMsg Authentication.SecretKeyFabricated

        Alien.SearchTracks ->
            data
                |> Json.decodeValue Json.string
                |> Result.withDefault ""
                |> Tracks.Search
                |> TracksMsg

        -----------------------------------------
        -- From UI
        -----------------------------------------
        Alien.ProcessSources ->
            -- Only proceed to the processing if we got all the necessary data,
            -- otherwise report an error in the UI.
            case Json.decodeValue Processing.argumentsDecoder data of
                Ok arguments ->
                    Process arguments

                Err err ->
                    report Alien.ProcessSources (Json.errorToString err)

        Alien.RedirectToBlockstackSignIn ->
            RedirectToBlockstackSignIn

        Alien.RemoveTracksBySourceId ->
            data
                |> Json.decodeValue Json.string
                |> Result.withDefault ""
                |> RemoveTracksBySourceId

        Alien.RemoveTracksFromCache ->
            Cmd (Brain.Ports.removeTracksFromCache data)

        Alien.SaveEnclosedUserData ->
            AuthenticationMsg (Authentication.SaveEnclosedData data)

        Alien.SaveFavourites ->
            SaveFavourites data

        Alien.SavePlaylists ->
            SavePlaylists data

        Alien.SaveSettings ->
            SaveSettings data

        Alien.SaveSources ->
            SaveSources data

        Alien.SaveTracks ->
            SaveTracks data

        Alien.SignIn ->
            AuthenticationMsg (Authentication.PerformSignIn data)

        Alien.SignOut ->
            AuthenticationMsg Authentication.PerformSignOut

        Alien.StoreTracksInCache ->
            Cmd (Brain.Ports.storeTracksInCache data)

        Alien.ToCache ->
            case Json.decodeValue Alien.hostDecoder data of
                Ok val ->
                    ToCache val

                Err err ->
                    report Alien.ToCache (Json.errorToString err)

        Alien.UpdateEncryptionKey ->
            case Json.decodeValue Json.string data of
                Ok passphrase ->
                    AuthenticationMsg (Authentication.FabricateSecretKey passphrase)

                Err _ ->
                    Bypass

        _ ->
            Bypass


translateAlienError : Alien.Tag -> String -> Msg
translateAlienError tag err =
    case tag of
        Alien.AuthAnonymous ->
            reportAuthError Alien.AuthAnonymous err "I found some encrypted data, but I couldn't decrypt it. Maybe you used the wrong passphrase?"

        Alien.AuthDropbox ->
            reportAuthError Alien.AuthDropbox err "I found some encrypted data, but I couldn't decrypt it. Maybe you used the wrong passphrase?"

        Alien.AuthIpfs ->
            reportAuthError Alien.AuthIpfs err "Something went wrong regarding the IPFS storage. Maybe you used the wrong passphrase, or your IPFS node is offline?"

        Alien.AuthRemoteStorage ->
            reportAuthError Alien.AuthRemoteStorage err "I found some encrypted data, but I couldn't decrypt it. Maybe you used the wrong passphrase?"

        Alien.AuthTextile ->
            reportAuthError Alien.AuthTextile err "Something went wrong regarding Textile. Maybe Textile isn't running?"

        _ ->
            case err of
                "db is undefined" ->
                    report tag "Can't connect to the browser's IndexedDB. FYI, this is __not supported in Firefox's private mode__."

                _ ->
                    report tag err


reportAuthError : Alien.Tag -> String -> String -> Msg
reportAuthError tag originalError fallbackError =
    case originalError of
        "MISSING_SECRET_KEY" ->
            [ ( "alienMethodTag", Alien.tagToJson tag )
            , ( "fallbackError", Json.Encode.string fallbackError )
            ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.MissingSecretKey
                |> NotifyUI

        _ ->
            report tag fallbackError


report : Alien.Tag -> String -> Msg
report tag err =
    err
        |> Alien.report tag
        |> NotifyUI
