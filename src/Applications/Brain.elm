module Brain exposing (main)

import Alien
import Brain.Ports
import Brain.Reply exposing (Reply(..))
import Brain.Sources.Processing as Processing
import Brain.Sources.Processing.Common as Processing
import Brain.Tracks as Tracks
import Brain.User.Layer as User
import Debouncer.Basic as Debouncer exposing (Debouncer)
import EverySet
import Json.Decode as Json
import Json.Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Playlists.Encoding as Playlists
import Return2 exposing (..)
import Return3
import Settings
import Sources.Encoding as Sources
import Sources.Processing as Processing
import Sources.Processing.Encoding as Processing
import Tracks
import Tracks.Encoding as Tracks
import Url
import User.Layer as User exposing (HypaethralBit(..))



-- 🧠


type alias Flags =
    { initialUrl : String }


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- 🌳


type alias Model =
    { hypaethralDebouncer : Debouncer HypaethralBit (List HypaethralBit)
    , hypaethralStorage : List HypaethralBit
    , hypaethralUserData : User.HypaethralData

    -- Children
    , processing : Processing.Model
    , tracks : Tracks.Model
    , userLayer : User.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        hypDebouncer =
            2.5
                |> Debouncer.fromSeconds
                |> Debouncer.debounce
                |> Debouncer.accumulateWith Debouncer.allInputs
                |> Debouncer.toDebouncer

        initialUrl =
            Maybe.withDefault
                { protocol = Url.Http
                , host = ""
                , port_ = Nothing
                , path = ""
                , query = Nothing
                , fragment = Nothing
                }
                (Url.fromString flags.initialUrl)
    in
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { hypaethralDebouncer = hypDebouncer
      , hypaethralStorage = []
      , hypaethralUserData = User.emptyHypaethralData

      -- Children
      , processing = Processing.initialModel
      , tracks = Tracks.initialModel
      , userLayer = User.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.batch
        [ Cmd.map UserLayerMsg (User.initialCommand initialUrl)
        , Cmd.map ProcessingMsg Processing.initialCommand
        ]
    )



-- 📣


type Msg
    = Bypass
      -----------------------------------------
      -- 🂡
      -----------------------------------------
    | Cmd (Cmd Msg)
    | ToCache Alien.Event
      -----------------------------------------
      -- Children
      -----------------------------------------
    | ProcessingMsg Processing.Msg
    | TracksMsg Tracks.Msg
    | UserLayerMsg User.Msg
      -----------------------------------------
      -- Sources & Tracks
      -----------------------------------------
    | Process Processing.Arguments
    | RemoveTracksBySourceId String
      -----------------------------------------
      -- User layer #1
      -----------------------------------------
    | SaveFavourites Json.Value
    | SavePlaylists Json.Value
    | SaveProgress Json.Value
    | SaveSettings Json.Value
    | SaveSources Json.Value
    | SaveTracks Json.Value
      -----------------------------------------
      -- User layer #2
      -----------------------------------------
    | RemoveEncryptionKey
    | SaveHypaethralDataSlowly (Debouncer.Msg HypaethralBit)
    | SaveHypaethralData (List HypaethralBit)
    | SaveNextHypaethralBit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            return model

        -----------------------------------------
        -- 🂡
        -----------------------------------------
        Cmd cmd ->
            returnWithModel model cmd

        ToCache alienEvent ->
            alienEvent
                |> Brain.Ports.toCache
                |> returnWithModel model

        -----------------------------------------
        -- Children
        -----------------------------------------
        UserLayerMsg User.PerformSignOut ->
            -- When signing out, remove all traces of the user's data.
            updateUserLayer
                { model | hypaethralUserData = User.emptyHypaethralData }
                User.PerformSignOut

        UserLayerMsg sub ->
            updateUserLayer model sub

        ProcessingMsg sub ->
            updateProcessing model sub

        TracksMsg sub ->
            updateTracks model sub

        -----------------------------------------
        -- Sources & Tracks
        -----------------------------------------
        Process { origin, sources } ->
            { origin = origin
            , sources = sources
            , tracks = model.hypaethralUserData.tracks
            }
                |> Processing.Process
                |> ProcessingMsg
                |> updateWithModel model

        RemoveTracksBySourceId sourceId ->
            model.hypaethralUserData.tracks
                |> Tracks.removeBySourceId sourceId
                |> .kept
                |> saveTracks model

        -----------------------------------------
        -- User layer #1
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
        SaveFavourites value ->
            value
                |> Json.decodeValue (Json.list Tracks.favouriteDecoder)
                |> Result.withDefault model.hypaethralUserData.favourites
                |> hypaethralLenses.setFavourites model
                |> saveHypaethralDataBitWithDelay Favourites

        SavePlaylists value ->
            value
                |> Json.decodeValue (Json.list Playlists.decoder)
                |> Result.withDefault model.hypaethralUserData.playlists
                |> hypaethralLenses.setPlaylists model
                |> saveHypaethralDataBitWithDelay Playlists

        SaveProgress value ->
            value
                |> Json.decodeValue (Json.dict Json.float)
                |> Result.withDefault model.hypaethralUserData.progress
                |> hypaethralLenses.setProgress model
                |> saveHypaethralDataBitWithDelay Progress

        SaveSettings value ->
            value
                |> Json.decodeValue (Json.map Just Settings.decoder)
                |> Result.withDefault model.hypaethralUserData.settings
                |> hypaethralLenses.setSettings model
                |> saveHypaethralDataBitWithDelay Settings

        SaveSources value ->
            value
                |> Json.decodeValue (Json.list Sources.decoder)
                |> Result.withDefault model.hypaethralUserData.sources
                |> hypaethralLenses.setSources model
                |> saveHypaethralDataBitWithDelay Sources

        SaveTracks value ->
            value
                |> Json.decodeValue (Json.list Tracks.trackDecoder)
                |> Result.withDefault model.hypaethralUserData.tracks
                |> saveTracks model

        -----------------------------------------
        -- User layer #2
        -----------------------------------------
        RemoveEncryptionKey ->
            Alien.AuthSecretKey
                |> Alien.trigger
                |> Brain.Ports.removeCache
                |> returnWithModel model
                |> andThen saveAllHypaethralData

        SaveHypaethralDataSlowly debouncerMsg ->
            Return3.wieldNested
                update
                { mapCmd = SaveHypaethralDataSlowly
                , mapModel = \child -> { model | hypaethralDebouncer = child }
                , update =
                    \dbMsg dbModel ->
                        let
                            ( m, c, r ) =
                                Debouncer.update dbMsg dbModel
                        in
                        ( m
                        , c
                        , r
                            |> Maybe.withDefault []
                            |> EverySet.fromList
                            |> EverySet.toList
                            |> SaveHypaethralData
                            |> List.singleton
                        )
                }
                { model = model.hypaethralDebouncer
                , msg = debouncerMsg
                }

        SaveHypaethralData bits ->
            case model.hypaethralStorage ++ bits of
                bit :: rest ->
                    model.hypaethralUserData
                        |> User.encodeHypaethralBit bit
                        |> User.SaveHypaethralData bit
                        |> UserLayerMsg
                        |> updateWithModel { model | hypaethralStorage = rest }

                _ ->
                    return model

        SaveNextHypaethralBit ->
            case model.hypaethralStorage of
                bit :: rest ->
                    model.hypaethralUserData
                        |> User.encodeHypaethralBit bit
                        |> User.SaveHypaethralData bit
                        |> UserLayerMsg
                        |> updateWithModel { model | hypaethralStorage = rest }

                _ ->
                    return model


saveTracks : Model -> List Tracks.Track -> Return Model Msg
saveTracks model tracks =
    tracks
        -- Store in model
        |> hypaethralLenses.setTracks model
        -- Update search index
        |> update
            (tracks
                |> Json.Encode.list Tracks.encodeTrack
                |> Tracks.UpdateSearchIndex
                |> TracksMsg
            )
        -- Save with delay
        |> andThen (saveHypaethralDataBitWithDelay Tracks)


updateWithModel : Model -> Msg -> Return Model Msg
updateWithModel model msg =
    update msg model



-- 📣  ░░  REPLIES


translateReply : Reply -> Model -> Return Model Msg
translateReply reply model =
    case reply of
        FabricatedNewSecretKey ->
            saveAllHypaethralData model

        -----------------------------------------
        -- Tracks
        -----------------------------------------
        AddTracks tracks ->
            tracks
                |> List.append model.hypaethralUserData.tracks
                |> saveTracks model

        RemoveTracksByPaths args ->
            model.hypaethralUserData.tracks
                |> Tracks.removeByPaths args
                |> .kept
                |> saveTracks model

        -----------------------------------------
        -- To UI
        -----------------------------------------
        GiveUI Alien.LoadHypaethralUserData data ->
            let
                decodedData =
                    data
                        |> User.decodeHypaethralData
                        |> Result.withDefault model.hypaethralUserData
            in
            data
                |> Alien.broadcast Alien.LoadHypaethralUserData
                |> Brain.Ports.toUI
                |> returnWithModel
                    { model | hypaethralUserData = decodedData }
                |> andThen
                    (decodedData.tracks
                        |> Json.Encode.list Tracks.encodeTrack
                        |> Tracks.UpdateSearchIndex
                        |> TracksMsg
                        |> update
                    )

        GiveUI tag data ->
            data
                |> Alien.broadcast tag
                |> Brain.Ports.toUI
                |> returnWithModel model

        NudgeUI Alien.ImportLegacyData ->
            model
                |> saveAllHypaethralData
                |> addCommand (Brain.Ports.toUI <| Alien.trigger Alien.ImportLegacyData)

        NudgeUI tag ->
            tag
                |> Alien.trigger
                |> Brain.Ports.toUI
                |> returnWithModel model



-- 📣  ░░  CHILDREN


updateUserLayer : Model -> User.Msg -> ( Model, Cmd Msg )
updateUserLayer model sub =
    Return3.wieldNested
        translateReply
        { mapCmd = UserLayerMsg
        , mapModel = \child -> { model | userLayer = child }
        , update = User.update
        }
        { model = model.userLayer
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
    , setProgress = makeHypaethralLens (\h p -> { h | progress = p })
    , setSettings = makeHypaethralLens (\h s -> { h | settings = s })
    , setSources = makeHypaethralLens (\h s -> { h | sources = s })
    , setTracks = makeHypaethralLens (\h t -> { h | tracks = t })
    }


makeHypaethralLens : (User.HypaethralData -> a -> User.HypaethralData) -> Model -> a -> Model
makeHypaethralLens setter model value =
    { model | hypaethralUserData = setter model.hypaethralUserData value }


saveAllHypaethralData : Model -> ( Model, Cmd Msg )
saveAllHypaethralData model =
    User.hypaethralBit.list
        |> List.map Tuple.second
        |> SaveHypaethralData
        |> updateWithModel model


saveHypaethralDataBitWithDelay : User.HypaethralBit -> Model -> ( Model, Cmd Msg )
saveHypaethralDataBitWithDelay bit model =
    bit
        |> Debouncer.provideInput
        |> SaveHypaethralDataSlowly
        |> updateWithModel model



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Brain.Ports.fromAlien alien
        , Brain.Ports.savedHypaethralBit (always SaveNextHypaethralBit)

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
            UserLayerMsg (User.HypaethralDataRetrieved data)

        Alien.AuthBlockstack ->
            UserLayerMsg (User.HypaethralDataRetrieved data)

        Alien.AuthDropbox ->
            UserLayerMsg (User.HypaethralDataRetrieved data)

        Alien.AuthEnclosedData ->
            UserLayerMsg (User.EnclosedDataRetrieved data)

        Alien.AuthIpfs ->
            UserLayerMsg (User.HypaethralDataRetrieved data)

        Alien.AuthMethod ->
            UserLayerMsg (User.MethodRetrieved data)

        Alien.AuthRemoteStorage ->
            UserLayerMsg (User.HypaethralDataRetrieved data)

        Alien.AuthTextile ->
            UserLayerMsg (User.HypaethralDataRetrieved data)

        Alien.FabricateSecretKey ->
            UserLayerMsg User.SecretKeyFabricated

        Alien.SearchTracks ->
            data
                |> Json.decodeValue Json.string
                |> Result.withDefault ""
                |> Tracks.Search
                |> TracksMsg

        -----------------------------------------
        -- From UI
        -----------------------------------------
        Alien.ImportLegacyData ->
            UserLayerMsg User.RetrieveLegacyHypaethralData

        Alien.ProcessSources ->
            -- Only proceed to the processing if we got all the necessary data,
            -- otherwise report an error in the UI.
            case Json.decodeValue Processing.argumentsDecoder data of
                Ok arguments ->
                    Process arguments

                Err err ->
                    report Alien.ProcessSources (Json.errorToString err)

        Alien.RedirectToBlockstackSignIn ->
            Cmd (Brain.Ports.redirectToBlockstackSignIn ())

        Alien.RemoveEncryptionKey ->
            RemoveEncryptionKey

        Alien.RemoveTracksBySourceId ->
            data
                |> Json.decodeValue Json.string
                |> Result.withDefault ""
                |> RemoveTracksBySourceId

        Alien.RemoveTracksFromCache ->
            Cmd (Brain.Ports.removeTracksFromCache data)

        Alien.SaveEnclosedUserData ->
            UserLayerMsg (User.SaveEnclosedData data)

        Alien.SaveFavourites ->
            SaveFavourites data

        Alien.SavePlaylists ->
            SavePlaylists data

        Alien.SaveProgress ->
            SaveProgress data

        Alien.SaveSettings ->
            SaveSettings data

        Alien.SaveSources ->
            SaveSources data

        Alien.SaveTracks ->
            SaveTracks data

        Alien.SignIn ->
            UserLayerMsg (User.PerformSignIn data)

        Alien.SignOut ->
            UserLayerMsg User.PerformSignOut

        Alien.StopProcessing ->
            ProcessingMsg Processing.StopProcessing

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
                    UserLayerMsg (User.FabricateSecretKey passphrase)

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
                |> Brain.Ports.toUI
                |> Cmd

        _ ->
            report tag fallbackError


report : Alien.Tag -> String -> Msg
report tag err =
    err
        |> Alien.report tag
        |> Brain.Ports.toUI
        |> Cmd
