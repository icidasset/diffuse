module Brain exposing (main)

import Alien
import Authentication exposing (HypaethralUserData)
import Brain.Authentication as Authentication
import Brain.Core exposing (..)
import Brain.Ports
import Brain.Reply as Reply exposing (Reply(..))
import Brain.Sources.Processing as Processing
import Brain.Sources.Processing.Common as Processing
import Brain.Tracks as Tracks
import Json.Decode as Json
import Json.Decode.Pipeline exposing (optional)
import Json.Encode
import Replying exposing (andThen, return)
import Sources.Encoding as Sources
import Sources.Processing.Encoding as Processing
import Tracks.Encoding as Tracks



-- 🧠


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- 🌳


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { authentication = Authentication.initialModel
      , hypaethralUserData = Authentication.emptyHypaethralUserData
      , processing = Processing.initialModel
      , tracks = Tracks.initialModel
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.batch
        [ Cmd.map AuthenticationMsg Authentication.initialCommand
        , Cmd.map ProcessingMsg Processing.initialCommand
        ]
    )



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            ( model
            , Cmd.none
            )

        NotifyUI alienEvent ->
            ( model
            , Brain.Ports.toUI alienEvent
            )

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
                        |> Authentication.decode
                        |> Result.withDefault model.hypaethralUserData
            in
            ( { model | hypaethralUserData = decodedData }
            , Brain.Ports.toUI (Alien.broadcast Alien.LoadHypaethralUserData value)
            )
                |> andThen updateSearchIndex

        SaveFavourites value ->
            value
                |> Json.decodeValue (Json.list Tracks.favouriteDecoder)
                |> Result.withDefault model.hypaethralUserData.favourites
                |> hypaethralLenses.setFavourites model
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
                |> updateSearchIndex
                |> andThen saveHypaethralData


updateSearchIndex : Model -> ( Model, Cmd Msg )
updateSearchIndex model =
    update
        (model.hypaethralUserData.tracks
            |> Tracks.UpdateSearchIndex
            |> TracksMsg
        )
        model



-- 📣  ░░  REPLIES


translateReply : Reply -> Msg
translateReply reply =
    case reply of
        Chill ->
            Bypass

        -----------------------------------------
        -- To UI
        -----------------------------------------
        GiveUI Alien.LoadHypaethralUserData data ->
            LoadHypaethralUserData data

        GiveUI tag data ->
            NotifyUI (Alien.broadcast tag data)

        NudgeUI tag ->
            NotifyUI (Alien.trigger tag)


updateChild =
    Replying.updateChild update translateReply



-- 📣  ░░  CHILDREN


updateAuthentication : Model -> Authentication.Msg -> ( Model, Cmd Msg )
updateAuthentication model sub =
    updateChild
        { mapCmd = AuthenticationMsg
        , mapModel = \child -> { model | authentication = child }
        , update = Authentication.update
        }
        { model = model.authentication
        , msg = sub
        }


updateProcessing : Model -> Processing.Msg -> ( Model, Cmd Msg )
updateProcessing model sub =
    updateChild
        { mapCmd = ProcessingMsg
        , mapModel = \child -> { model | processing = child }
        , update = Processing.update
        }
        { model = model.processing
        , msg = sub
        }


updateTracks : Model -> Tracks.Msg -> ( Model, Cmd Msg )
updateTracks model sub =
    updateChild
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
    , setSources = makeHypaethralLens (\h s -> { h | sources = s })
    , setTracks = makeHypaethralLens (\h t -> { h | tracks = t })
    }


makeHypaethralLens : (HypaethralUserData -> a -> HypaethralUserData) -> Model -> a -> Model
makeHypaethralLens setter model value =
    { model | hypaethralUserData = setter model.hypaethralUserData value }


saveHypaethralData : Model -> ( Model, Cmd Msg )
saveHypaethralData model =
    model.hypaethralUserData
        |> Authentication.encodeHypaethralUserData
        |> Authentication.SaveHypaethralData
        |> AuthenticationMsg
        |> (\msg -> update msg model)



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Brain.Ports.fromCache translateAlienEvent
        , Brain.Ports.fromUI translateAlienEvent

        -----------------------------------------
        -- Children
        -----------------------------------------
        , Sub.map ProcessingMsg (Processing.subscriptions model.processing)
        ]


translateAlienEvent : Alien.Event -> Msg
translateAlienEvent event =
    case Alien.tagFromString event.tag of
        Just Alien.AuthAnonymous ->
            AuthenticationMsg (Authentication.HypaethralDataRetrieved event.data)

        Just Alien.AuthMethod ->
            AuthenticationMsg (Authentication.MethodRetrieved event.data)

        Just Alien.ProcessSources ->
            -- Only proceed to the processing if we got all the necessary data,
            -- otherwise report an error in the UI.
            case Json.decodeValue Processing.argumentsDecoder event.data of
                Ok arguments ->
                    arguments
                        |> Processing.Process
                        |> ProcessingMsg

                Err error ->
                    error
                        |> Json.errorToString
                        |> Alien.report Alien.ReportGenericError
                        |> NotifyUI

        Just Alien.SaveEnclosedUserData ->
            AuthenticationMsg (Authentication.SaveEnclosedData event.data)

        Just Alien.SaveFavourites ->
            SaveFavourites event.data

        Just Alien.SaveSources ->
            SaveSources event.data

        Just Alien.SaveTracks ->
            SaveTracks event.data

        Just Alien.SearchTracks ->
            event.data
                |> Json.decodeValue Json.string
                |> Result.withDefault ""
                |> Tracks.Search
                |> TracksMsg

        Just Alien.SignIn ->
            AuthenticationMsg (Authentication.PerformSignIn event.data)

        Just Alien.SignOut ->
            AuthenticationMsg Authentication.PerformSignOut

        _ ->
            Bypass
