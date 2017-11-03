module Slave.State exposing (..)

import Date
import Dict.Ext as Dict
import Json.Decode as Decode exposing (..)
import Response exposing (..)
import Response.Ext as Response exposing (do)
import Slave.Ports as Ports
import Slave.Translations as Translations
import Slave.Types exposing (..)
import Sources.Encoding
import Task
import Time
import Tracks.Encoding exposing (trackDecoder)
import Types as TopLevel exposing (AlienEvent)


-- Children

import Sources.Processing.State
import Sources.Processing.Types


-- 💧


initialModel : Model
initialModel =
    { sourceProcessing = Sources.Processing.State.initialModel
    , timestamp = Date.fromTime 0
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.batch
        [ -- Time
          Task.perform SetTimestamp Time.now
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Extraterrestrial
        ------------------------------------
        --
        -- Process sources
        --
        Extraterrestrial ProcessSources (Ok result) ->
            case decodeValue (dict string) result of
                -- 🚀
                --
                Ok dict ->
                    (!)
                        model
                        [ let
                            sources =
                                dict
                                    |> Dict.fetch "sources" "[]"
                                    |> Decode.decodeString (Decode.list Sources.Encoding.decoder)
                                    |> Result.withDefault []

                            tracks =
                                dict
                                    |> Dict.fetch "tracks" "[]"
                                    |> Decode.decodeString (Decode.list trackDecoder)
                                    |> Result.withDefault []
                          in
                            tracks
                                |> Sources.Processing.Types.Process sources
                                |> SourceProcessingMsg
                                |> do
                        ]

                -- ⚠️
                --
                Err _ ->
                    (!) model []

        Extraterrestrial ProcessSources (Err _) ->
            (!) model []

        --
        -- Ignore other
        --
        Extraterrestrial _ _ ->
            (!) model []

        ------------------------------------
        -- Children
        ------------------------------------
        SourceProcessingMsg sub ->
            model.sourceProcessing
                |> Sources.Processing.State.update sub
                |> mapModel (\x -> { model | sourceProcessing = x })

        ------------------------------------
        -- Time
        ------------------------------------
        SetTimestamp time ->
            let
                stamp =
                    Date.fromTime time

                sourceProcessing =
                    model.sourceProcessing
            in
                (!)
                    { model
                        | sourceProcessing = { sourceProcessing | timestamp = stamp }
                        , timestamp = stamp
                    }
                    []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ -- Time
          Time.every (1 * Time.minute) SetTimestamp

        -- Talking to the outside world
        , Ports.incoming handleAlienEvent
        ]


handleAlienEvent : AlienEvent -> Msg
handleAlienEvent event =
    Extraterrestrial
        (Translations.stringToAlienMessage event.tag)
        (case event.error of
            Just err ->
                Err err

            Nothing ->
                Ok event.data
        )
